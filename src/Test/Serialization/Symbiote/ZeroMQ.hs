{-# LANGUAGE
    DataKinds
  , RankNTypes
  , FlexibleContexts
  , ScopedTypeVariables
  #-}

{-|

Module: Test.Serialization.Symbiote.ZeroMQ
Copyright: (c) 2019 Athan Clark
License: BSD-3-Style
Maintainer: athan.clark@gmail.com
Portability: GHC

Use these functions to communicate over a Peer-to-Peer ZeroMQ socket.

-}

module Test.Serialization.Symbiote.ZeroMQ where


import Test.Serialization.Symbiote
  (firstPeer, secondPeer, SymbioteT, defaultFailure, defaultProgress, nullProgress, Topic, Failure)
import Test.Serialization.Symbiote.Debug (Debug (..))

import qualified Data.ByteString as BS
import qualified Data.Serialize as Cereal
import Data.List.NonEmpty (NonEmpty (..))
import Data.Singleton.Class (Extractable)
import Control.Monad (forever)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control.Aligned (MonadBaseControl)
import Control.Concurrent.Async (Async, cancel)
import Control.Concurrent.Chan.Scope (Scope (Read, Write))
import Control.Concurrent.Chan.Extra (readOnly, writeOnly)
import Control.Concurrent.STM (TChan, TMVar, newTChanIO, writeTChan, readTChan, newEmptyTMVarIO, putTMVar, takeTMVar, atomically)
import Control.Concurrent.STM.TChan.Typed (TChanRW, newTChanRW, writeTChanRW, readTChanRW)
import Control.Concurrent.Threaded.Hash (threaded)
import System.ZMQ4 (Router (..), Dealer (..))
import System.ZMQ4.Monadic (runZMQ, async)
import System.ZMQ4.Simple (ZMQIdent, socket, bind, send, receive, connect, setUUIDIdentity)



secondPeerZeroMQ :: MonadIO m
                 => MonadBaseControl IO m stM
                 => MonadCatch m
                 => Extractable stM
                 => ZeroMQParams
                 -> Debug
                 -> SymbioteT BS.ByteString m () -- ^ Tests registered
                 -> m ()
secondPeerZeroMQ params debug = peerZeroMQ params debug secondPeer

firstPeerZeroMQ :: MonadIO m
                => MonadBaseControl IO m stM
                => MonadCatch m
                => Extractable stM
                => ZeroMQParams
                -> Debug
                -> SymbioteT BS.ByteString m () -- ^ Tests registered
                -> m ()
firstPeerZeroMQ params debug = peerZeroMQ params debug firstPeer


data ZeroMQServerOrClient
  = ZeroMQServer
  | ZeroMQClient

data ZeroMQParams = ZeroMQParams
  { zmqHost :: String
  , zmqServerOrClient :: ZeroMQServerOrClient
  }


-- | ZeroMQ can only work on 'BS.ByteString's
peerZeroMQ :: forall m stM them me
            . MonadIO m
           => MonadBaseControl IO m stM
           => Extractable stM
           => Show (them BS.ByteString)
           => Cereal.Serialize (me BS.ByteString)
           => Cereal.Serialize (them BS.ByteString)
           => ZeroMQParams
           -> Debug
           -> ( (me BS.ByteString -> m ())
             -> m (them BS.ByteString)
             -> (Topic -> m ())
             -> (Failure them BS.ByteString -> m ())
             -> (Topic -> Float -> m ())
             -> SymbioteT BS.ByteString m ()
             -> m ()
              ) -- ^ Encode and send, receive and decode, on success, on failure, on progress, and test set
           -> SymbioteT BS.ByteString m () -- ^ Tests registered
           -> m ()
peerZeroMQ params debug peer tests = do
  case params of
    ZeroMQParams host ZeroMQServer -> do
      (incoming :: TChanRW 'Write (ZMQIdent, them BS.ByteString)) <- writeOnly <$> liftIO (atomically newTChanRW)
      -- the process that gets invoked for each new thread. Writes to a @me BS.ByteString@ and reads from a @them BS.ByteString@.
      let process :: TChanRW 'Read (them BS.ByteString) -> TChanRW 'Write (me BS.ByteString) -> m ()
          process inputs outputs =
            let encodeAndSend :: me BS.ByteString -> m ()
                encodeAndSend x = liftIO $ atomically $ writeTChanRW outputs x

                receiveAndDecode :: m (them BS.ByteString)
                receiveAndDecode = liftIO $ atomically $ readTChanRW inputs

                onSuccess t = liftIO $ putStrLn $ "Topic finished: " ++ show t
                onFailure = liftIO . defaultFailure
                onProgress t n = case debug of
                  NoDebug -> nullProgress t n
                  _ -> liftIO (defaultProgress t n)
            in  peer encodeAndSend receiveAndDecode onSuccess onFailure onProgress tests
      -- manage invoked threads
      ( mainThread
        , outgoing :: TChanRW 'Read (ZMQIdent, me BS.ByteString)
        ) <- threaded incoming process
      (outgoingThreadVar :: TMVar (Async ())) <- liftIO newEmptyTMVarIO

      -- forever bind to ZeroMQ
      runZMQ $ do
        s <- socket Router Dealer
        bind s host

        -- sending loop (separate thread)
        outgoingThread <- async $ forever $ do
          (ident, x) <- liftIO (atomically (readTChanRW outgoing))
          send ident s ((Cereal.encode x) :| [])
        liftIO (atomically (putTMVar outgoingThreadVar outgoingThread))
        -- FIXME kill this thread on ZeroMQ / main thread death - a 'la Async.link & bind?

        -- receiving loop (current thread)
        forever $ do
          mX <- receive s
          case mX of
            Nothing -> liftIO $ putStrLn "got nothin"
            Just (ident,x :| _) -> case Cereal.decode x of
              Left e -> error $ "couldn't decode: " ++ e
              Right x' -> liftIO (atomically (writeTChanRW incoming (ident,x')))
      -- liftIO (cancel zThread) - never dies automatically?

    ZeroMQParams host ZeroMQClient -> do
      (outgoing :: TChan (me BS.ByteString)) <- liftIO newTChanIO
      (incoming :: TChan (them BS.ByteString)) <- liftIO newTChanIO
      (outgoingThreadVar :: TMVar (Async ())) <- liftIO newEmptyTMVarIO

      -- thread that connects and communicates with ZeroMQ
      zThread <- runZMQ $ async $ do
        s <- socket Dealer Router
        setUUIDIdentity s
        connect s host

        -- sending loop (separate thread)
        outgoingThread <- async $ forever $ do
          x <- liftIO (atomically (readTChan outgoing))
          send () s ((Cereal.encode x) :| [])
        liftIO (atomically (putTMVar outgoingThreadVar outgoingThread))
        -- FIXME kill this thread on ZeroMQ death - a 'la Async.link & bind?

        -- receiving loop (current thread)
        forever $ do
          mX <- receive s
          case mX of
            Nothing -> liftIO (putStrLn "got nothin")
            Just ((),x :| _) -> case Cereal.decode x of
              Left e -> error ("couldn't decode: " ++ e)
              Right x' -> liftIO (atomically (writeTChan incoming x'))

      -- main loop (current thread, continues when finished)
      let encodeAndSend :: me BS.ByteString -> m ()
          encodeAndSend x = liftIO (atomically (writeTChan outgoing x))

          receiveAndDecode :: m (them BS.ByteString)
          receiveAndDecode = liftIO (atomically (readTChan incoming))

          onSuccess t = liftIO $ putStrLn $ "Topic finished: " ++ show t
          onFailure = liftIO . defaultFailure
          onProgress t n = case debug of
            NoDebug -> nullProgress t n
            _ -> liftIO (defaultProgress t n)
      peer encodeAndSend receiveAndDecode onSuccess onFailure onProgress tests

      -- kill ZeroMQ thread
      liftIO (cancel zThread)
