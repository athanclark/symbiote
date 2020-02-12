{-# LANGUAGE
    DataKinds
  , RankNTypes
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
import Test.Serialization.Symbiote.Debug (Debug (..), Network (..))
import Test.Serialization.Symbiote.WebSocket.Ident (newWebSocketIdent, WithWebSocketIdent (..), WebSocketIdent)

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as Json
import Data.Serialize (Serialize)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Serialize as Cereal
import Data.Singleton.Class (Extractable)
import Control.Monad (forever, void)
import Control.Monad.Trans.Control.Aligned (MonadBaseControl, liftBaseWith)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadCatch)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
  ( TChan, TMVar, newTChanIO, readTChan, writeTChan, atomically
  , newEmptyTMVarIO, putTMVar, takeTMVar)
import Control.Concurrent.STM.TChan.Typed (TChanRW, newTChanRW, writeTChanRW, readTChanRW)
import Control.Concurrent.Async (async, cancel, Async)
import Control.Concurrent.Chan.Scope (Scope (Read, Write))
import Control.Concurrent.Chan.Extra (writeOnly)
import Control.Concurrent.Threaded.Hash (threaded)
import Network.WebSockets.Simple
  (WebSocketsApp (..), WebSocketsAppParams (..), toClientAppTString, toClientAppTBinary, dimap', dimapJson, dimapStringify)
import Network.WebSockets.Simple.Logger (logStdout)
import Network.WebSockets.Trans (ClientAppT)
import System.Timeout (timeout)


secondPeerWebSocketLazyByteString :: MonadIO m
                                  => MonadBaseControl IO m stM
                                  => MonadCatch m
                                  => Extractable stM
                                  => WebSocketParams
                                  -> Debug
                                  -> SymbioteT LBS.ByteString m () -- ^ Tests registered
                                  -> m ()
secondPeerWebSocketLazyByteString params debug = peerWebSocketLazyByteString params debug secondPeer

firstPeerWebSocketLazyByteString :: MonadIO m
                                 => MonadBaseControl IO m stM
                                 => MonadCatch m
                                 => Extractable stM
                                 => WebSocketParams
                                 -> Debug
                                 -> SymbioteT LBS.ByteString m () -- ^ Tests registered
                                 -> m ()
firstPeerWebSocketLazyByteString params debug = peerWebSocketLazyByteString params debug firstPeer

secondPeerWebSocketByteString :: MonadIO m
                              => MonadBaseControl IO m stM
                              => MonadCatch m
                              => Extractable stM
                              => WebSocketParams
                              -> Debug
                              -> SymbioteT BS.ByteString m () -- ^ Tests registered
                              -> m ()
secondPeerWebSocketByteString params debug = peerWebSocketByteString params debug secondPeer

firstPeerWebSocketByteString :: MonadIO m
                             => MonadBaseControl IO m stM
                             => MonadCatch m
                             => Extractable stM
                             => WebSocketParams
                             -> Debug
                             -> SymbioteT BS.ByteString m () -- ^ Tests registered
                             -> m ()
firstPeerWebSocketByteString params debug = peerWebSocketByteString params debug firstPeer

secondPeerWebSocketJson :: MonadIO m
                        => MonadBaseControl IO m stM
                        => MonadCatch m
                        => Extractable stM
                        => WebSocketParams
                        -> Debug
                        -> SymbioteT Json.Value m () -- ^ Tests registered
                        -> m ()
secondPeerWebSocketJson params debug = peerWebSocketJson params debug secondPeer

firstPeerWebSocketJson :: MonadIO m
                       => MonadBaseControl IO m stM
                       => MonadCatch m
                       => Extractable stM
                       => WebSocketParams
                       -> Debug
                       -> SymbioteT Json.Value m () -- ^ Tests registered
                       -> m ()
firstPeerWebSocketJson params debug = peerWebSocketJson params debug firstPeer

peerWebSocketLazyByteString :: forall m stM them me
                             . MonadIO m
                            => MonadBaseControl IO m stM
                            => MonadCatch m
                            => Extractable stM
                            => Show (them LBS.ByteString)
                            => Serialize (me LBS.ByteString)
                            => Serialize (them LBS.ByteString)
                            => WebSocketParams
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
peerWebSocketLazyByteString (WebSocketParams runWebSocket clientOrServer network) debug peer tests
  | network == Private || (network == Public && clientOrServer == WebSocketClient) = do
      (outgoing :: TChan (me LBS.ByteString)) <- liftIO newTChanIO
      (incoming :: TChan (them LBS.ByteString)) <- liftIO newTChanIO
      (outgoingThreadVar :: TMVar (Async ())) <- liftIO newEmptyTMVarIO

      let encodeAndSend x = liftIO $ atomically $ writeTChan outgoing x
          receiveAndDecode = liftIO $ atomically $ readTChan incoming
          onSuccess t = liftIO $ putStrLn $ "WebSocket Lazy ByteString Topic finished: " ++ show t
          onFailure = liftIO . defaultFailure
          onProgress t n = case debug of
            NoDebug -> nullProgress t n
            _ -> liftIO (defaultProgress t n)

          webSocket :: forall a b. Serialize a => Serialize b => WebSocketsApp IO a b -> IO ()
          webSocket app =
            runWebSocket $ toClientAppTBinary $
              ( case debug of
                  FullDebug -> logStdout
                  _ -> id
              ) $ dimap' receive Cereal.encodeLazy app
            where
              receive :: LBS.ByteString -> IO a
              receive buf = do
                let eX = Cereal.decodeLazy buf
                case eX of
                  Left e -> do
                    putStrLn ("Can't parse buffer: " ++ show buf)
                    error e
                  Right x -> pure x
      wsThread <- case network of
        Private -> do
          let onOpen :: WebSocketsAppParams IO (me LBS.ByteString) -> IO ()
              onOpen WebSocketsAppParams{send} = do
                outgoingThread <- async $ forever $ atomically (readTChan outgoing) >>= send
                atomically (putTMVar outgoingThreadVar outgoingThread)
              app :: WebSocketsApp IO (them LBS.ByteString) (me LBS.ByteString)
              app = WebSocketsApp
                { onClose = \_ _ -> do
                    outgoingThread <- atomically (takeTMVar outgoingThreadVar)
                    cancel outgoingThread
                , onReceive = \_ -> atomically . writeTChan incoming
                , onOpen
                }
          liftIO (async (webSocket app))
        Public -> do
          ident <- liftIO newWebSocketIdent
          let mkWsMessage = WithWebSocketIdent ident

              onOpen :: WebSocketsAppParams IO (WithWebSocketIdent (me LBS.ByteString)) -> IO ()
              onOpen WebSocketsAppParams{send} = do
                outgoingThread <- async $ forever $ do
                  x <- atomically (readTChan outgoing)
                  send (mkWsMessage x)
                atomically (putTMVar outgoingThreadVar outgoingThread)
              app :: WebSocketsApp IO (WithWebSocketIdent (them LBS.ByteString)) (WithWebSocketIdent (me LBS.ByteString))
              app = WebSocketsApp
                { onClose = \_ _ -> do
                    outgoingThread <- atomically (takeTMVar outgoingThreadVar)
                    cancel outgoingThread
                , onReceive = \_ (WithWebSocketIdent _ x) -> atomically (writeTChan incoming x)
                , onOpen
                }
          liftIO (async (webSocket app))

      peer encodeAndSend receiveAndDecode onSuccess onFailure onProgress tests
      liftIO (cancel wsThread)
  | otherwise = do
      (incoming :: TChanRW 'Write (WebSocketIdent, them LBS.ByteString))
        <- writeOnly <$> liftIO (atomically newTChanRW)

      let process :: TChanRW 'Read (them LBS.ByteString) -> TChanRW 'Write (me LBS.ByteString) -> m ()
          process inputs outputs = void $ liftBaseWith $ \runInBase -> timeout 10000000 $ runInBase $ do
            let encodeAndSend :: me LBS.ByteString -> m ()
                encodeAndSend x = liftIO $ atomically $ writeTChanRW outputs x
                receiveAndDecode :: m (them LBS.ByteString)
                receiveAndDecode = liftIO $ atomically $ readTChanRW inputs

                onSuccess t = liftIO $ putStrLn $ "WebSocket Lazy ByteString Topic finished: " ++ show t
                onFailure = liftIO . defaultFailure
                onProgress t n = case debug of
                  NoDebug -> nullProgress t n
                  _ -> liftIO (defaultProgress t n)
            peer encodeAndSend receiveAndDecode onSuccess onFailure onProgress tests
            liftIO (threadDelay 1000000)

      ( _
        , outgoing :: TChanRW 'Read (WebSocketIdent, me LBS.ByteString)
        ) <- threaded incoming process
      (outgoingThreadVar :: TMVar (Async ())) <- liftIO newEmptyTMVarIO


      let onOpen :: WebSocketsAppParams IO (WithWebSocketIdent (me LBS.ByteString)) -> IO ()
          onOpen WebSocketsAppParams{send} = do
            outgoingThread <- async $ forever $ do
              (ident, x) <- atomically (readTChanRW outgoing)
              send (WithWebSocketIdent ident x)
            atomically (putTMVar outgoingThreadVar outgoingThread)
          app :: WebSocketsApp IO
                   (WithWebSocketIdent (them LBS.ByteString))
                   (WithWebSocketIdent (me LBS.ByteString))
          app = WebSocketsApp
            { onClose = \_ _ -> do
                outgoingThread <- atomically (takeTMVar outgoingThreadVar)
                cancel outgoingThread
            , onReceive = \_ (WithWebSocketIdent ident x) -> atomically (writeTChanRW incoming (ident, x))
            , onOpen
            }
          webSocket :: IO ()
          webSocket =
            runWebSocket $ toClientAppTBinary $
              ( case debug of
                  FullDebug -> logStdout
                  _ -> id
              ) $ dimap' receive Cereal.encodeLazy app
            where
              receive :: LBS.ByteString -> IO (WithWebSocketIdent (them LBS.ByteString))
              receive buf = do
                let eX = Cereal.decodeLazy buf
                case eX of
                  Left e -> do
                    putStrLn ("Can't parse buffer: " ++ show buf)
                    error e
                  Right x -> pure x
      liftIO webSocket

peerWebSocketByteString :: forall m stM them me
                         . MonadIO m
                        => MonadBaseControl IO m stM
                        => MonadCatch m
                        => Extractable stM
                        => Show (them BS.ByteString)
                        => Serialize (me BS.ByteString)
                        => Serialize (them BS.ByteString)
                        => WebSocketParams
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
peerWebSocketByteString (WebSocketParams runWebSocket clientOrServer network) debug peer tests
  | network == Private || (network == Public && clientOrServer == WebSocketClient) = do
      (outgoing :: TChan (me BS.ByteString)) <- liftIO newTChanIO
      (incoming :: TChan (them BS.ByteString)) <- liftIO newTChanIO
      (outgoingThreadVar :: TMVar (Async ())) <- liftIO newEmptyTMVarIO

      let encodeAndSend x = liftIO $ atomically $ writeTChan outgoing x
          receiveAndDecode = liftIO $ atomically $ readTChan incoming
          onSuccess t = liftIO $ putStrLn $ "WebSocket ByteString Topic finished: " ++ show t
          onFailure = liftIO . defaultFailure
          onProgress t n = case debug of
            NoDebug -> nullProgress t n
            _ -> liftIO (defaultProgress t n)

          webSocket :: forall a b. Serialize a => Serialize b => WebSocketsApp IO a b -> IO ()
          webSocket app =
            runWebSocket $ toClientAppTBinary $ toLazy $
              ( case debug of
                  FullDebug -> logStdout
                  _ -> id
              ) $ dimap' receive Cereal.encode app
            where
              receive :: BS.ByteString -> IO a
              receive buf = do
                let eX = Cereal.decode buf
                case eX of
                  Left e -> do
                    putStrLn ("Can't parse buffer: " ++ show buf)
                    error e
                  Right x -> pure x

              toLazy :: WebSocketsApp IO BS.ByteString BS.ByteString
                    -> WebSocketsApp IO LBS.ByteString LBS.ByteString
              toLazy = dimap' r s
                where
                  r = pure . LBS.toStrict
                  s = LBS.fromStrict
      wsThread <- case network of
        Private -> do
          let onOpen :: WebSocketsAppParams IO (me BS.ByteString) -> IO ()
              onOpen WebSocketsAppParams{send} = do
                outgoingThread <- async $ forever $ atomically (readTChan outgoing) >>= send
                atomically (putTMVar outgoingThreadVar outgoingThread)
              app :: WebSocketsApp IO (them BS.ByteString) (me BS.ByteString)
              app = WebSocketsApp
                { onClose = \_ _ -> do
                    outgoingThread <- atomically (takeTMVar outgoingThreadVar)
                    cancel outgoingThread
                , onReceive = \_ -> atomically . writeTChan incoming
                , onOpen
                }
          liftIO (async (webSocket app))
        Public -> do
          ident <- liftIO newWebSocketIdent
          let mkWsMessage = WithWebSocketIdent ident

              onOpen :: WebSocketsAppParams IO (WithWebSocketIdent (me BS.ByteString)) -> IO ()
              onOpen WebSocketsAppParams{send} = do
                outgoingThread <- async $ forever $ do
                  x <- atomically (readTChan outgoing)
                  send (mkWsMessage x)
                atomically (putTMVar outgoingThreadVar outgoingThread)
              app :: WebSocketsApp IO (WithWebSocketIdent (them BS.ByteString)) (WithWebSocketIdent (me BS.ByteString))
              app = WebSocketsApp
                { onClose = \_ _ -> do
                    outgoingThread <- atomically (takeTMVar outgoingThreadVar)
                    cancel outgoingThread
                , onReceive = \_ (WithWebSocketIdent _ x) -> atomically (writeTChan incoming x)
                , onOpen
                }
          liftIO (async (webSocket app))

      peer encodeAndSend receiveAndDecode onSuccess onFailure onProgress tests
      liftIO (cancel wsThread)
  | otherwise = do
      (incoming :: TChanRW 'Write (WebSocketIdent, them BS.ByteString))
        <- writeOnly <$> liftIO (atomically newTChanRW)

      let process :: TChanRW 'Read (them BS.ByteString) -> TChanRW 'Write (me BS.ByteString) -> m ()
          process inputs outputs = void $ liftBaseWith $ \runInBase -> timeout 10000000 $ runInBase $ do
            let encodeAndSend :: me BS.ByteString -> m ()
                encodeAndSend x = liftIO $ atomically $ writeTChanRW outputs x
                receiveAndDecode :: m (them BS.ByteString)
                receiveAndDecode = liftIO $ atomically $ readTChanRW inputs

                onSuccess t = liftIO $ putStrLn $ "WebSocket ByteString Topic finished: " ++ show t
                onFailure = liftIO . defaultFailure
                onProgress t n = case debug of
                  NoDebug -> nullProgress t n
                  _ -> liftIO (defaultProgress t n)
            peer encodeAndSend receiveAndDecode onSuccess onFailure onProgress tests
            liftIO (threadDelay 1000000)

      ( _
        , outgoing :: TChanRW 'Read (WebSocketIdent, me BS.ByteString)
        ) <- threaded incoming process
      (outgoingThreadVar :: TMVar (Async ())) <- liftIO newEmptyTMVarIO


      let onOpen :: WebSocketsAppParams IO (WithWebSocketIdent (me BS.ByteString)) -> IO ()
          onOpen WebSocketsAppParams{send} = do
            outgoingThread <- async $ forever $ do
              (ident, x) <- atomically (readTChanRW outgoing)
              send (WithWebSocketIdent ident x)
            atomically (putTMVar outgoingThreadVar outgoingThread)
          app :: WebSocketsApp IO
                   (WithWebSocketIdent (them BS.ByteString))
                   (WithWebSocketIdent (me BS.ByteString))
          app = WebSocketsApp
            { onClose = \_ _ -> do
                outgoingThread <- atomically (takeTMVar outgoingThreadVar)
                cancel outgoingThread
            , onReceive = \_ (WithWebSocketIdent ident x) -> atomically (writeTChanRW incoming (ident, x))
            , onOpen
            }
          webSocket :: IO ()
          webSocket =
            runWebSocket $ toClientAppTBinary $ toLazy $
              ( case debug of
                  FullDebug -> logStdout
                  _ -> id
              ) $ dimap' receive Cereal.encode app
            where
              receive :: BS.ByteString -> IO (WithWebSocketIdent (them BS.ByteString))
              receive buf = do
                let eX = Cereal.decode buf
                case eX of
                  Left e -> do
                    putStrLn ("Can't parse buffer: " ++ show buf)
                    error e
                  Right x -> pure x

              toLazy :: WebSocketsApp IO BS.ByteString BS.ByteString
                     -> WebSocketsApp IO LBS.ByteString LBS.ByteString
              toLazy = dimap' r s
                where
                  r = pure . LBS.toStrict
                  s = LBS.fromStrict
      liftIO webSocket


-- WebSockets can work with both Json 'Value's and 'BS.ByteString's
peerWebSocketJson :: forall m stM them me
                   . MonadIO m
                  => MonadBaseControl IO m stM
                  => Extractable stM
                  => Show (them Json.Value)
                  => ToJSON (me Json.Value)
                  => FromJSON (them Json.Value)
                  => WebSocketParams
                  -> Debug
                  -> ( (me Json.Value -> m ())
                    -> m (them Json.Value)
                    -> (Topic -> m ())
                    -> (Failure them Json.Value -> m ())
                    -> (Topic -> Float -> m ())
                    -> SymbioteT Json.Value m ()
                    -> m ()
                    ) -- ^ Encode and send, receive and decode, on success, on failure, on progress, and test set
                  -> SymbioteT Json.Value m () -- ^ Tests registered
                  -> m ()
peerWebSocketJson (WebSocketParams runWebSocket clientOrServer network) debug peer tests
  | network == Private || (network == Public && clientOrServer == WebSocketClient) = do
      (outgoing :: TChan (me Json.Value)) <- liftIO newTChanIO
      (incoming :: TChan (them Json.Value)) <- liftIO newTChanIO
      (outgoingThreadVar :: TMVar (Async ())) <- liftIO newEmptyTMVarIO

      let encodeAndSend x = liftIO $ atomically $ writeTChan outgoing x
          receiveAndDecode = liftIO $ atomically $ readTChan incoming
          onSuccess t = liftIO $ putStrLn $ "WebSocket Json Topic finished: " ++ show t
          onFailure = liftIO . defaultFailure
          onProgress t n = case debug of
            NoDebug -> nullProgress t n
            _ -> liftIO (defaultProgress t n)
          webSocket :: forall a b. FromJSON a => ToJSON b => WebSocketsApp IO a b -> IO ()
          webSocket =
            runWebSocket
            . toClientAppTString
            . ( case debug of
                  FullDebug -> logStdout
                  _ -> id
              )
            . dimapStringify
            . dimapJson

      wsThread <- case network of
        Private -> do
          let onOpen :: WebSocketsAppParams IO (me Json.Value) -> IO ()
              onOpen WebSocketsAppParams{send} = do
                outgoingThread <- async $ forever $ atomically (readTChan outgoing) >>= send
                atomically (putTMVar outgoingThreadVar outgoingThread)
              app :: WebSocketsApp IO (them Json.Value) (me Json.Value)
              app = WebSocketsApp
                { onClose = \_ _ -> do
                    outgoingThread <- atomically (takeTMVar outgoingThreadVar)
                    cancel outgoingThread
                , onReceive = \_ -> atomically . writeTChan incoming
                , onOpen
                }

          liftIO (async (webSocket app))
        Public -> do
          ident <- liftIO newWebSocketIdent
          let mkWsMessage = WithWebSocketIdent ident

              onOpen :: WebSocketsAppParams IO (WithWebSocketIdent (me Json.Value)) -> IO ()
              onOpen WebSocketsAppParams{send} = do
                outgoingThread <- async $ forever $ do
                  x <- atomically (readTChan outgoing)
                  send (mkWsMessage x)
                atomically (putTMVar outgoingThreadVar outgoingThread)
              app :: WebSocketsApp IO (WithWebSocketIdent (them Json.Value)) (WithWebSocketIdent (me Json.Value))
              app = WebSocketsApp
                { onClose = \_ _ -> do
                    outgoingThread <- atomically (takeTMVar outgoingThreadVar)
                    cancel outgoingThread
                , onReceive = \_ (WithWebSocketIdent _ x) -> atomically (writeTChan incoming x)
                , onOpen
                }
          liftIO (async (webSocket app))

      peer encodeAndSend receiveAndDecode onSuccess onFailure onProgress tests
      liftIO (cancel wsThread)
  | otherwise = do
      (incoming :: TChanRW 'Write (WebSocketIdent, them Json.Value))
        <- writeOnly <$> liftIO (atomically newTChanRW)

      let process :: TChanRW 'Read (them Json.Value) -> TChanRW 'Write (me Json.Value) -> m ()
          process inputs outputs = void $ liftBaseWith $ \runInBase -> timeout 10000000 $ runInBase $ do
            let encodeAndSend :: me Json.Value -> m ()
                encodeAndSend x = liftIO $ atomically $ writeTChanRW outputs x
                receiveAndDecode :: m (them Json.Value)
                receiveAndDecode = liftIO $ atomically $ readTChanRW inputs

                onSuccess t = liftIO $ putStrLn $ "WebSocket Json Topic finished: " ++ show t
                onFailure = liftIO . defaultFailure
                onProgress t n = case debug of
                  NoDebug -> nullProgress t n
                  _ -> liftIO (defaultProgress t n)
            peer encodeAndSend receiveAndDecode onSuccess onFailure onProgress tests
            liftIO (threadDelay 1000000)

      ( _
        , outgoing :: TChanRW 'Read (WebSocketIdent, me Json.Value)
        ) <- threaded incoming process
      (outgoingThreadVar :: TMVar (Async ())) <- liftIO newEmptyTMVarIO


      let onOpen :: WebSocketsAppParams IO (WithWebSocketIdent (me Json.Value)) -> IO ()
          onOpen WebSocketsAppParams{send} = do
            outgoingThread <- async $ forever $ do
              (ident, x) <- atomically (readTChanRW outgoing)
              send (WithWebSocketIdent ident x)
            atomically (putTMVar outgoingThreadVar outgoingThread)
          app :: WebSocketsApp IO
                   (WithWebSocketIdent (them Json.Value))
                   (WithWebSocketIdent (me Json.Value))
          app = WebSocketsApp
            { onClose = \_ _ -> do
                outgoingThread <- atomically (takeTMVar outgoingThreadVar)
                cancel outgoingThread
            , onReceive = \_ (WithWebSocketIdent ident x) -> atomically (writeTChanRW incoming (ident, x))
            , onOpen
            }
          webSocket :: WebSocketsApp IO
                         (WithWebSocketIdent (them Json.Value))
                         (WithWebSocketIdent (me Json.Value))
                    -> IO ()
          webSocket =
            runWebSocket
            . toClientAppTString
            . ( case debug of
                  FullDebug -> logStdout
                  _ -> id
              )
            . dimapStringify
            . dimapJson
      liftIO (webSocket app)



data WebSocketServerOrClient
  = WebSocketServer
  | WebSocketClient
  deriving (Eq)

data WebSocketParams = WebSocketParams
  { runWebSocket            :: ClientAppT IO () -> IO () -- ^ Run the generated WebSocketsApp
  , webSocketServerOrClient :: WebSocketServerOrClient
  , webSocketNetwork        :: Network
  }
