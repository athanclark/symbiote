{-# LANGUAGE
    RankNTypes
  , NamedFieldPuns
  , FlexibleContexts
  , ScopedTypeVariables
  #-}

module Test.Serialization.Symbiote.WebSocket where

import Test.Serialization.Symbiote
  (firstPeer, secondPeer, SymbioteT, defaultFailure, defaultProgress, Topic, Failure)

import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize)
import Data.ByteString.Lazy (ByteString)
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


data Debug = Debug | NoDebug


secondPeerWebSocketByteString :: MonadIO m
                              => MonadBaseControl IO m stM
                              => MonadCatch m
                              => Extractable stM
                              => Show s
                              => Serialize s
                              => (ClientAppT IO () -> IO ())
                              -> Debug
                              -> SymbioteT s m ()
                              -> m ()
secondPeerWebSocketByteString host debug = peerWebSocketByteString host debug secondPeer

firstPeerWebSocketByteString :: MonadIO m
                             => MonadBaseControl IO m stM
                             => MonadCatch m
                             => Extractable stM
                             => Show s
                             => Serialize s
                             => (ClientAppT IO () -> IO ())
                             -> Debug
                             -> SymbioteT s m ()
                             -> m ()
firstPeerWebSocketByteString host debug = peerWebSocketByteString host debug firstPeer

secondPeerWebSocketJson :: MonadIO m
                        => MonadBaseControl IO m stM
                        => MonadCatch m
                        => Extractable stM
                        => Show s
                        => ToJSON s
                        => FromJSON s
                        => (ClientAppT IO () -> IO ())
                        -> Debug
                        -> SymbioteT s m ()
                        -> m ()
secondPeerWebSocketJson host debug = peerWebSocketJson host debug secondPeer

firstPeerWebSocketJson :: MonadIO m
                       => MonadBaseControl IO m stM
                       => MonadCatch m
                       => Extractable stM
                       => Show s
                       => ToJSON s
                       => FromJSON s
                       => (ClientAppT IO () -> IO ())
                       -> Debug
                       -> SymbioteT s m ()
                       -> m ()
firstPeerWebSocketJson host debug = peerWebSocketJson host debug firstPeer


peerWebSocketByteString :: forall m stM s them me
                         . MonadIO m
                        => MonadBaseControl IO m stM
                        => MonadCatch m
                        => Extractable stM
                        => Show s
                        => Show (them s)
                        => Serialize (me s)
                        => Serialize (them s)
                        => (ClientAppT IO () -> IO ())
                        -> Debug
                        -> ( (me s -> m ())
                          -> m (them s)
                          -> (Topic -> m ())
                          -> (Failure them s -> m ())
                          -> (Topic -> Float -> m ())
                          -> SymbioteT s m ()
                          -> m ()
                          )
                        -> SymbioteT s m ()
                        -> m ()
peerWebSocketByteString runClientAppT debug = peerWebSocket $ \app ->
  runClientAppT $ toClientAppTBinary $
    ( case debug of
        Debug -> logStdout
        NoDebug -> id
    ) $ dimap' receive send app
  where
    receive :: ByteString -> IO (them s)
    receive buf = do
      let eX = Cereal.decodeLazy buf
      case eX of
        Left e -> do
          putStrLn $ "Can't parse buffer: " ++ show buf
          error e
        Right x -> pure x

    send :: me s -> ByteString
    send = Cereal.encodeLazy


peerWebSocketJson :: MonadIO m
                  => MonadBaseControl IO m stM
                  => Extractable stM
                  => MonadCatch m
                  => Show s
                  => Show (them s)
                  => ToJSON (me s)
                  => FromJSON (them s)
                  => (ClientAppT IO () -> IO ())
                  -> Debug
                  -> ( (me s -> m ())
                    -> m (them s)
                    -> (Topic -> m ())
                    -> (Failure them s -> m ())
                    -> (Topic -> Float -> m ())
                    -> SymbioteT s m ()
                    -> m ()
                    )
                  -> SymbioteT s m ()
                  -> m ()
peerWebSocketJson runClientAppT debug = peerWebSocket
  ( runClientAppT
    . toClientAppTString
    . ( case debug of
          Debug -> logStdout
          NoDebug -> id
      )
    . dimapStringify
    . dimapJson
  )


peerWebSocket :: forall m stM s them me
               . MonadIO m
              => MonadBaseControl IO m stM
              => Show (them s)
              => Show s
              => ( WebSocketsApp IO (them s) (me s)
                -> IO ()
                 )
              -> ( (me s -> m ())
                -> m (them s)
                -> (Topic -> m ())
                -> (Failure them s -> m ())
                -> (Topic -> Float -> m ())
                -> SymbioteT s m ()
                -> m ()
                 )
              -> SymbioteT s m ()
              -> m ()
peerWebSocket webSocket peer tests = do
  (outgoing :: TChan (me s)) <- liftIO newTChanIO
  (incoming :: TChan (them s)) <- liftIO newTChanIO
  (outgoingThreadVar :: TMVar (Async ())) <- liftIO newEmptyTMVarIO
  let encodeAndSend x = liftIO $ atomically $ writeTChan outgoing x
      receiveAndDecode = liftIO $ atomically $ readTChan incoming
      onSuccess t = liftIO $ putStrLn $ "Topic finished: " ++ show t
      onFailure = liftIO . defaultFailure
      onProgress t n = liftIO (defaultProgress t n)

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
  liftIO (wait wsThread)
