{-# LANGUAGE
    RankNTypes
  , NamedFieldPuns
  , FlexibleContexts
  , ScopedTypeVariables
  #-}

{-|

Module: Test.Serialization.Symbiote.WebSocket
Copyright: (c) 2019 Athan Clark
License: BSD-3-Style
Maintainer: athan.clark@gmail.com
Portability: GHC

Use these functions to communicate over a WebSocket as your peer-to-peer communication mechanism.

-}


module Test.Serialization.Symbiote.WebSocket where

import Test.Serialization.Symbiote
  (firstPeer, secondPeer, SymbioteT, defaultFailure, defaultProgress, nullProgress, Topic, Failure)
import Test.Serialization.Symbiote.Debug (Debug (..))

import Data.Aeson (ToJSON, FromJSON, Value)
import Data.Serialize (Serialize)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Serialize as Cereal
import Data.Singleton.Class (Extractable)
import Control.Monad (forever, void)
import Control.Monad.Trans.Control.Aligned (MonadBaseControl, liftBaseWith)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadCatch)
import Control.Concurrent.STM
  ( TChan, TMVar, newTChanIO, readTChan, writeTChan, atomically
  , newEmptyTMVarIO, putTMVar, takeTMVar)
import Control.Concurrent.Async (async, cancel, wait, Async)
import Network.WebSockets.Simple
  (WebSocketsApp (..), WebSocketsAppParams (..), toClientAppTString, toClientAppTBinary, dimap', dimapJson, dimapStringify)
import Network.WebSockets.Simple.Logger (logStdout)
import Network.WebSockets.Trans (ClientAppT)


secondPeerWebSocketLazyByteString :: MonadIO m
                                  => MonadBaseControl IO m stM
                                  => MonadCatch m
                                  => Extractable stM
                                  => (ClientAppT IO () -> IO ()) -- ^ Run the generated WebSocket client
                                  -> Debug
                                  -> SymbioteT LBS.ByteString m () -- ^ Tests registered
                                  -> m ()
secondPeerWebSocketLazyByteString host debug = peerWebSocketLazyByteString host debug secondPeer

firstPeerWebSocketLazyByteString :: MonadIO m
                                 => MonadBaseControl IO m stM
                                 => MonadCatch m
                                 => Extractable stM
                                 => (ClientAppT IO () -> IO ()) -- ^ Run the generated WebSocket client
                                 -> Debug
                                 -> SymbioteT LBS.ByteString m () -- ^ Tests registered
                                 -> m ()
firstPeerWebSocketLazyByteString host debug = peerWebSocketLazyByteString host debug firstPeer

secondPeerWebSocketByteString :: MonadIO m
                              => MonadBaseControl IO m stM
                              => MonadCatch m
                              => Extractable stM
                              => (ClientAppT IO () -> IO ()) -- ^ Run the generated WebSocket client
                              -> Debug
                              -> SymbioteT BS.ByteString m () -- ^ Tests registered
                              -> m ()
secondPeerWebSocketByteString host debug = peerWebSocketByteString host debug secondPeer

firstPeerWebSocketByteString :: MonadIO m
                             => MonadBaseControl IO m stM
                             => MonadCatch m
                             => Extractable stM
                             => (ClientAppT IO () -> IO ()) -- ^ Run the generated WebSocket client
                             -> Debug
                             -> SymbioteT BS.ByteString m () -- ^ Tests registered
                             -> m ()
firstPeerWebSocketByteString host debug = peerWebSocketByteString host debug firstPeer

secondPeerWebSocketJson :: MonadIO m
                        => MonadBaseControl IO m stM
                        => MonadCatch m
                        => Extractable stM
                        => (ClientAppT IO () -> IO ()) -- ^ Run the generated WebSocket client
                        -> Debug
                        -> SymbioteT Value m () -- ^ Tests registered
                        -> m ()
secondPeerWebSocketJson host debug = peerWebSocketJson host debug secondPeer

firstPeerWebSocketJson :: MonadIO m
                       => MonadBaseControl IO m stM
                       => MonadCatch m
                       => Extractable stM
                       => (ClientAppT IO () -> IO ()) -- ^ Run the generated WebSocket client
                       -> Debug
                       -> SymbioteT Value m () -- ^ Tests registered
                       -> m ()
firstPeerWebSocketJson host debug = peerWebSocketJson host debug firstPeer

peerWebSocketLazyByteString :: forall m stM them me
                             . MonadIO m
                            => MonadBaseControl IO m stM
                            => MonadCatch m
                            => Extractable stM
                            => Show (them LBS.ByteString)
                            => Serialize (me LBS.ByteString)
                            => Serialize (them LBS.ByteString)
                            => (ClientAppT IO () -> IO ()) -- ^ Run the generated WebSocket client
                            -> Debug
                            -> ( (me LBS.ByteString -> m ())
                              -> m (them LBS.ByteString)
                              -> (Topic -> m ())
                              -> (Failure them LBS.ByteString -> m ())
                              -> (Topic -> Float -> m ())
                              -> SymbioteT LBS.ByteString m ()
                              -> m ()
                              )
                            -> SymbioteT LBS.ByteString m () -- ^ Tests registered
                            -> m ()
peerWebSocketLazyByteString runClientAppT debug = peerWebSocket go debug
  where
    go :: WebSocketsApp IO (them LBS.ByteString) (me LBS.ByteString) -> IO ()
    go app =
      runClientAppT $ toClientAppTBinary $
        ( case debug of
            FullDebug -> logStdout
            _ -> id
        ) $ dimap' receive Cereal.encodeLazy app
      where
        receive :: LBS.ByteString -> IO (them LBS.ByteString)
        receive buf = do
          let eX = Cereal.decodeLazy buf
          case eX of
            Left e -> do
              putStrLn $ "Can't parse buffer: " ++ show buf
              error e
            Right x -> pure x

peerWebSocketByteString :: forall m stM them me
                         . MonadIO m
                        => MonadBaseControl IO m stM
                        => MonadCatch m
                        => Extractable stM
                        => Show (them BS.ByteString)
                        => Serialize (me BS.ByteString)
                        => Serialize (them BS.ByteString)
                        => (ClientAppT IO () -> IO ()) -- ^ Run the generated WebSocket client
                        -> Debug
                        -> ( (me BS.ByteString -> m ())
                          -> m (them BS.ByteString)
                          -> (Topic -> m ())
                          -> (Failure them BS.ByteString -> m ())
                          -> (Topic -> Float -> m ())
                          -> SymbioteT BS.ByteString m ()
                          -> m ()
                          )
                        -> SymbioteT BS.ByteString m () -- ^ Tests registered
                        -> m ()
peerWebSocketByteString runClientAppT debug = peerWebSocket go debug
  where
    go :: WebSocketsApp IO (them BS.ByteString) (me BS.ByteString) -> IO ()
    go app =
      runClientAppT $ toClientAppTBinary $ toLazy $
        ( case debug of
            FullDebug -> logStdout
            _ -> id
        ) $ dimap' receive Cereal.encode app
      where
        receive :: BS.ByteString -> IO (them BS.ByteString)
        receive buf = do
          let eX = Cereal.decode buf
          case eX of
            Left e -> do
              putStrLn $ "Can't parse buffer: " ++ show buf
              error e
            Right x -> pure x

        toLazy :: WebSocketsApp IO BS.ByteString BS.ByteString -> WebSocketsApp IO LBS.ByteString LBS.ByteString
        toLazy = dimap' r s
          where
            r = pure . LBS.toStrict
            s = LBS.fromStrict


peerWebSocketJson :: MonadIO m
                  => MonadBaseControl IO m stM
                  => Extractable stM
                  => MonadCatch m
                  => Show (them Value)
                  => ToJSON (me Value)
                  => FromJSON (them Value)
                  => (ClientAppT IO () -> IO ()) -- ^ Run the generated WebSocket client
                  -> Debug
                  -> ( (me Value -> m ())
                    -> m (them Value)
                    -> (Topic -> m ())
                    -> (Failure them Value -> m ())
                    -> (Topic -> Float -> m ())
                    -> SymbioteT Value m ()
                    -> m ()
                    )
                  -> SymbioteT Value m () -- ^ Tests registered
                  -> m ()
peerWebSocketJson runClientAppT debug = peerWebSocket
  ( runClientAppT
    . toClientAppTString
    . ( case debug of
          FullDebug -> logStdout
          _ -> id
      )
    . dimapStringify
    . dimapJson
  )
  debug

peerWebSocket :: forall m stM s them me
               . MonadIO m
              => MonadBaseControl IO m stM
              => Show (them s)
              => Show s
              => ( WebSocketsApp IO (them s) (me s)
                -> IO ()
                 ) -- ^ Run the generated WebSocketsApp
              -> Debug
              -> ( (me s -> m ())
                -> m (them s)
                -> (Topic -> m ())
                -> (Failure them s -> m ())
                -> (Topic -> Float -> m ())
                -> SymbioteT s m ()
                -> m ()
                 )
              -> SymbioteT s m () -- ^ Tests registered
              -> m ()
peerWebSocket webSocket debug peer tests = do
  (outgoing :: TChan (me s)) <- liftIO newTChanIO
  (incoming :: TChan (them s)) <- liftIO newTChanIO
  (outgoingThreadVar :: TMVar (Async ())) <- liftIO newEmptyTMVarIO
  let encodeAndSend x = liftIO $ atomically $ writeTChan outgoing x
      receiveAndDecode = liftIO $ atomically $ readTChan incoming
      onSuccess t = liftIO $ putStrLn $ "Topic finished: " ++ show t
      onFailure = liftIO . defaultFailure
      onProgress t n = case debug of
        NoDebug -> nullProgress t n
        _ -> liftIO (defaultProgress t n)

      onOpen :: WebSocketsAppParams IO (me s) -> IO ()
      onOpen WebSocketsAppParams{close,send} = do
        outgoingThread <- async $ forever $ atomically (readTChan outgoing) >>= send
        atomically (putTMVar outgoingThreadVar outgoingThread)
      app :: WebSocketsApp IO (them s) (me s)
      app = WebSocketsApp
        { onClose = \_ _ -> do
            outgoingThread <- atomically (takeTMVar outgoingThreadVar)
            cancel outgoingThread
        , onReceive = \_ -> atomically . writeTChan incoming
        , onOpen
        }
  wsThread <- liftIO (async (webSocket app))
  peer encodeAndSend receiveAndDecode onSuccess onFailure onProgress tests
  liftIO (cancel wsThread)
