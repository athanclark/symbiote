{-# LANGUAGE
    RankNTypes
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
import Control.Concurrent.STM (TChan, TMVar, newTChanIO, writeTChan, readTChan, newEmptyTMVarIO, putTMVar, takeTMVar, atomically)
import System.ZMQ4 (Router (..), Dealer (..))
import System.ZMQ4.Monadic (runZMQ, async)
import System.ZMQ4.Simple (socket, bind, send, receive, connect, setUUIDIdentity)



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
  (outgoing :: TChan (me BS.ByteString)) <- liftIO newTChanIO
  (incoming :: TChan (them BS.ByteString)) <- liftIO newTChanIO
  (outgoingThreadVar :: TMVar (Async ())) <- liftIO newEmptyTMVarIO
  let encodeAndSend x = liftIO $ atomically $ writeTChan outgoing x
      receiveAndDecode = liftIO $ atomically $ readTChan incoming
      onSuccess t = liftIO $ putStrLn $ "Topic finished: " ++ show t
      onFailure = liftIO . defaultFailure
      onProgress t n = case debug of
        NoDebug -> nullProgress t n
        _ -> liftIO (defaultProgress t n)
  zThread <- runZMQ $ async $ do
    case params of
      ZeroMQParams host ZeroMQServer -> do
        s <- socket Router Dealer -- FIXME TODO this is only a server, need to support client too
        bind s host               --    <------
        outgoingThread <- async $ forever $ do
          x <- liftIO (atomically (readTChan outgoing)) -- FIXME TMapChan of ZMQIdents? Set of threads?
          send () s ((Cereal.encode x) :| [])
        liftIO (atomically (putTMVar outgoingThreadVar outgoingThread))
        forever $ do
          mX <- receive s
          case mX of
            Nothing -> liftIO $ putStrLn "got nothin"
            Just ((),x :| _) -> case Cereal.decode x of
              Left e -> error $ "couldn't decode: " ++ e
              Right x' -> liftIO (atomically (writeTChan incoming x'))
      ZeroMQParams host ZeroMQClient -> do
        s <- socket Dealer Router
        setUUIDIdentity s
        connect s host
        outgoingThread <- async $ forever $ do
          x <- liftIO (atomically (readTChan outgoing))
          send () s ((Cereal.encode x) :| [])
        liftIO (atomically (putTMVar outgoingThreadVar outgoingThread))
        forever $ do
          mX <- receive s
          case mX of
            Nothing -> liftIO $ putStrLn "got nothin"
            Just ((),x :| _) -> case Cereal.decode x of
              Left e -> error $ "couldn't decode: " ++ e
              Right x' -> liftIO (atomically (writeTChan incoming x'))
  peer encodeAndSend receiveAndDecode onSuccess onFailure onProgress tests
  liftIO (cancel zThread)
