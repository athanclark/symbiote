{-# LANGUAGE
    RankNTypes
  , NamedFieldPuns
  , FlexibleContexts
  , ScopedTypeVariables
  #-}

module Test.Serialization.Symbiote.WebSocket where

import Test.Serialization.Symbiote
  (firstPeer, secondPeer, SymbioteT, defaultFailure, nullProgress, Topic, Failure)

import Data.Aeson (ToJSON, FromJSON)
import Data.Singleton.Class (Extractable)
import Control.Monad (forever, void)
import Control.Monad.Trans.Control.Aligned (MonadBaseControl, liftBaseWith)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadCatch)
import Control.Concurrent.STM (TChan, newTChanIO, readTChan, writeTChan, atomically)
import Control.Concurrent.Async (async, cancel)
import Network.WebSockets.Simple (WebSocketsApp (..), WebSocketsAppParams (..), toClientAppT)
import Network.WebSockets.Trans (ClientAppT)
-- import Effect.Class (class MonadEffect, liftEffect)
-- import Effect.Console (log)
-- import Effect.Aff (Aff, runAff_, makeAff, nonCanceler)
-- import Effect.Aff.Class (class MonadAff, liftAff)
-- import Effect.Unsafe (unsafePerformEffect)
-- import Effect.Exception (throwException, throw)
-- import Data.Maybe (Maybe (..))
-- import Data.Either (Either (Right))
-- import Data.Functor.Singleton (class SingletonFunctor, liftBaseWith_)
-- import Data.Argonaut (class EncodeJson, class DecodeJson)
-- import Data.ArrayBuffer.Types (ArrayBuffer)
-- import Data.ArrayBuffer.Class
--   ( class EncodeArrayBuffer, class DecodeArrayBuffer
--   , encodeArrayBuffer, decodeArrayBuffer, class DynamicByteLength)
-- import Control.Monad.Trans.Control (class MonadBaseControl)
-- import Queue.One (Queue, READ, WRITE)
-- import Queue.One (new,put,draw,on,del) as Q
-- import WebSocket (dimap', WebSocketsApp (..), Capabilities)
-- import WebSocket.Class (newWebSocket, newWebSocketBinary)
-- import Debug.Trace (traceM)



-- secondPeerWebSocketArrayBuffer :: forall m stM s
--                         . MonadEffect m
--                        => MonadAff m
--                        => MonadBaseControl Aff m stM
--                        => SingletonFunctor stM
--                        => Show s
--                        => EncodeArrayBuffer s
--                        => DecodeArrayBuffer s
--                        => DynamicByteLength s
--                        => String
--                        -> SymbioteT s m Unit
--                        -> m Unit
-- secondPeerWebSocketArrayBuffer host = peerWebSocketArrayBuffer host secondPeer

-- firstPeerWebSocketArrayBuffer :: forall m stM s
--                         . MonadEffect m
--                        => MonadAff m
--                        => MonadBaseControl Aff m stM
--                        => SingletonFunctor stM
--                        => Show s
--                        => EncodeArrayBuffer s
--                        => DecodeArrayBuffer s
--                        => DynamicByteLength s
--                        => String
--                        -> SymbioteT s m Unit
--                        -> m Unit
-- firstPeerWebSocketArrayBuffer host = peerWebSocketArrayBuffer host firstPeer

-- secondPeerWebSocketJson :: forall m stM s
--                         . MonadEffect m
--                        => MonadAff m
--                        => MonadBaseControl Aff m stM
--                        => SingletonFunctor stM
--                        => Show s
--                        => EncodeJson s
--                        => DecodeJson s
--                        => String
--                        -> SymbioteT s m Unit
--                        -> m Unit
-- secondPeerWebSocketJson host = peerWebSocketJson host secondPeer

-- firstPeerWebSocketJson :: forall m stM s
--                         . MonadEffect m
--                        => MonadAff m
--                        => MonadBaseControl Aff m stM
--                        => SingletonFunctor stM
--                        => Show s
--                        => EncodeJson s
--                        => DecodeJson s
--                        => String
--                        -> SymbioteT s m Unit
--                        -> m Unit
-- firstPeerWebSocketJson host = peerWebSocketJson host firstPeer


-- peerWebSocketArrayBuffer :: forall m stM s them me
--                           . MonadEffect m
--                          => MonadAff m
--                          => MonadBaseControl Aff m stM
--                          => SingletonFunctor stM
--                          => Show s
--                          => EncodeArrayBuffer (me s)
--                          => DecodeArrayBuffer (them s)
--                          => DynamicByteLength (me s)
--                          => String
--                          -> ( (me s -> m Unit)
--                            -> m (them s)
--                            -> (Topic -> m Unit)
--                            -> (Failure them s -> m Unit)
--                            -> (Topic -> Number -> m Unit)
--                            -> SymbioteT s m Unit
--                            -> m Unit
--                            )
--                          -> SymbioteT s m Unit
--                          -> m Unit
-- peerWebSocketArrayBuffer host = peerWebSocket \app ->
--   newWebSocketBinary host [] (dimap' receive send app)
--   where
--     receive :: ArrayBuffer -> m (them s)
--     receive buf = liftEffect do
--       mX <- decodeArrayBuffer buf
--       case mX of
--         Nothing -> do
--           log "Can't parse buffer:"
--           traceM buf
--           throw "Failed."
--         Just x -> pure x

--     send :: me s -> ArrayBuffer
--     send x = unsafePerformEffect do
--       buf <- encodeArrayBuffer x
--       pure buf


peerWebSocketJson :: MonadIO m
                  => MonadBaseControl IO m stM
                  => Extractable stM
                  => MonadCatch m
                  => Show s
                  => Show (them s)
                  => ToJSON (me s)
                  => FromJSON (them s)
                  => (ClientAppT m () -> m ())
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
peerWebSocketJson runClientAppT = peerWebSocket (runClientAppT . toClientAppT)


peerWebSocket :: forall m stM s them me
               . MonadIO m
              => MonadBaseControl IO m stM
              => Show (them s)
              => Show s
              => ( WebSocketsApp m (them s) (me s)
                -> m ()
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
  (done :: TChan ()) <- liftIO newTChanIO
  let encodeAndSend x = liftIO $ atomically $ writeTChan outgoing x
      receiveAndDecode = liftIO $ atomically $ readTChan incoming
      onSuccess t = liftIO $ putStrLn $ "Topic finished: " ++ show t
      onFailure f = liftIO $ defaultFailure f
      onProgress = nullProgress

      onOpen :: WebSocketsAppParams m (me s) -> m ()
      onOpen WebSocketsAppParams{close,send} = liftBaseWith $ \runInBase -> do
        outgoingThread <- async $ forever $ do
          x <- atomically $ readTChan outgoing
          void (runInBase (send x))
        () <- atomically $ readTChan done
        cancel outgoingThread
        void (runInBase close)
      app :: WebSocketsApp m (them s) (me s)
      app = WebSocketsApp
        { onClose = \_ _ -> pure ()
        , onReceive = \_ x -> liftIO $ atomically $ writeTChan incoming x
        , onOpen
        }
  webSocket app
  peer encodeAndSend receiveAndDecode onSuccess onFailure onProgress tests
  liftIO $ atomically $ writeTChan done ()
