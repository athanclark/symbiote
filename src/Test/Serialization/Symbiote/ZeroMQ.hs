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
import Test.Serialization.Symbiote.Debug (Debug (..), Network (..))

import qualified Data.ByteString as BS
import qualified Data.Serialize as Cereal
import Data.List.NonEmpty (NonEmpty (..))
import Data.Singleton.Class (Extractable)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control.Aligned (MonadBaseControl, liftBaseWith)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel)
import Control.Concurrent.Chan.Scope (Scope (Read, Write))
import Control.Concurrent.Chan.Extra (writeOnly)
import Control.Concurrent.STM (TChan, newTChanIO, writeTChan, readTChan, atomically)
import Control.Concurrent.STM.TChan.Typed (TChanRW, newTChanRW, writeTChanRW, readTChanRW)
import Control.Concurrent.Threaded.Hash (threaded)
import qualified System.ZMQ4 as Z
import System.ZMQ4 (Router (..), Dealer (..), Pair (..))
import System.ZMQ4.Monadic (runZMQ, async)
import System.ZMQ4.Simple (ZMQIdent, socket, bind, send, receive, connect, setUUIDIdentity)
import System.Timeout (timeout)
import Unsafe.Coerce (unsafeCoerce)



secondPeerZeroMQ :: MonadIO m
                 => MonadBaseControl IO m stM
                 => Extractable stM
                 => ZeroMQParams
                 -> Debug
                 -> SymbioteT BS.ByteString m () -- ^ Tests registered
                 -> m ()
secondPeerZeroMQ params debug = peerZeroMQ params debug secondPeer

firstPeerZeroMQ :: MonadIO m
                => MonadBaseControl IO m stM
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
  { zmqHost           :: String
  , zmqServerOrClient :: ZeroMQServerOrClient
  , zmqNetwork        :: Network
  , zmqOnInit         :: forall a. Z.Socket a -> IO () -- ^ To manually adjust for encryption settings
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
peerZeroMQ (ZeroMQParams host clientOrServer network onInit) debug peer tests =
  case (network,clientOrServer) of
    (Public,ZeroMQServer) -> do
      (incoming :: TChanRW 'Write (ZMQIdent, them BS.ByteString)) <- writeOnly <$> liftIO (atomically newTChanRW)
      -- the process that gets invoked for each new thread. Writes to a @me BS.ByteString@ and reads from a @them BS.ByteString@.
      let process :: TChanRW 'Read (them BS.ByteString) -> TChanRW 'Write (me BS.ByteString) -> m ()
          process inputs outputs = void $ liftBaseWith $ \runInBase -> timeout 10000000 $ runInBase $ do
            let encodeAndSend :: me BS.ByteString -> m ()
                encodeAndSend x = liftIO $ atomically $ writeTChanRW outputs x

                receiveAndDecode :: m (them BS.ByteString)
                receiveAndDecode = liftIO $ atomically $ readTChanRW inputs

                onSuccess t = liftIO $ putStrLn $ "ZeroMQ Topic finished: " ++ show t
                onFailure = liftIO . defaultFailure
                onProgress t n = case debug of
                  NoDebug -> nullProgress t n
                  _ -> liftIO (defaultProgress t n)
            peer encodeAndSend receiveAndDecode onSuccess onFailure onProgress tests
            liftIO (threadDelay 1000000)

      -- manage invoked threads
      ( _
        , outgoing :: TChanRW 'Read (ZMQIdent, me BS.ByteString)
        ) <- threaded incoming process

      -- forever bind to ZeroMQ
      runZMQ $ do
        s <- socket Router Dealer
        liftIO (onInit (unsafeCoerce s))
        bind s host

        -- sending loop (separate thread)
        void $ async $ forever $ do
          (ident, x) <- liftIO (atomically (readTChanRW outgoing))
          send ident s ((Cereal.encode x) :| [])

        -- receiving loop (current thread)
        forever $ do
          mX <- receive s
          case mX of
            Nothing -> liftIO $ putStrLn "got nothin"
            Just (ident,x :| _) -> case Cereal.decode x of
              Left e -> error $ "couldn't decode: " ++ e
              Right x' -> do
                liftIO (atomically (writeTChanRW incoming (ident,x')))
      -- liftIO (cancel zThread) - never dies automatically?

    _ -> do
      (outgoing :: TChan (me BS.ByteString)) <- liftIO newTChanIO
      (incoming :: TChan (them BS.ByteString)) <- liftIO newTChanIO
      let encodeAndSend :: me BS.ByteString -> m ()
          encodeAndSend x = liftIO (atomically (writeTChan outgoing x))

          receiveAndDecode :: m (them BS.ByteString)
          receiveAndDecode = liftIO (atomically (readTChan incoming))

          onSuccess t = liftIO $ putStrLn $ "ZeroMQ Topic finished: " ++ show t
          onFailure = liftIO . defaultFailure
          onProgress t n = case debug of
            NoDebug -> nullProgress t n
            _ -> liftIO (defaultProgress t n)

          -- sending loop (separate thread)
          sendingThread s =
            void $ async $ forever $ do
              x <- liftIO (atomically (readTChan outgoing))
              send () s ((Cereal.encode x) :| [])

          -- receiving loop (current thread)
          receivingLoop s = forever $ do
            mX <- receive s
            case mX of
              Nothing -> liftIO (putStrLn "got nothin")
              Just ((),x :| _) -> case Cereal.decode x of
                Left e -> error ("couldn't decode: " ++ e)
                Right x' -> do
                  liftIO (atomically (writeTChan incoming x'))

      -- thread that connects and communicates with ZeroMQ
      zThread <- case network of
        -- is a ZeroMQClient
        Public -> runZMQ $ async $ do
          s <- socket Dealer Router
          liftIO (onInit (unsafeCoerce s))
          setUUIDIdentity s
          connect s host

          sendingThread s

          receivingLoop s
        Private -> case clientOrServer of
          ZeroMQServer -> runZMQ $ async $ do
            s <- socket Pair Pair
            liftIO (onInit (unsafeCoerce s))
            bind s host

            sendingThread s

            receivingLoop s
          ZeroMQClient -> runZMQ $ async $ do
            s <- socket Pair Pair
            liftIO (onInit (unsafeCoerce s))
            connect s host

            sendingThread s

            receivingLoop s

      -- main loop (current thread, continues when finished)
      peer encodeAndSend receiveAndDecode onSuccess onFailure onProgress tests

      -- kill ZeroMQ thread
      liftIO (cancel zThread)
