{-# LANGUAGE
    MultiParamTypeClasses
  , TypeFamilies
  , FlexibleInstances
  , FlexibleContexts
  , OverloadedStrings
  #-}

module Spec.Protocol where

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Serialize as Cereal
import qualified Data.Serialize.Get as Cereal
import qualified Data.Serialize.Put as Cereal
import qualified Data.ByteString as BS
import Data.Proxy
import Control.Monad.IO.Class (liftIO)
import Test.Serialization.Symbiote
  (SymbioteOperation (..), Generating, Operating, First, Second, Topic, SymbioteT, register)
import Test.Serialization.Symbiote.WebSocket (firstPeerWebSocketJson, firstPeerWebSocketByteString, Debug (..))
import Test.Serialization.Symbiote.Aeson ()
import Test.Serialization.Symbiote.Cereal ()
import Test.QuickCheck (Arbitrary (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Network.WebSockets.Connection (defaultConnectionOptions)
import Network.WebSockets.Simple (accept)
import Network.WebSockets.Trans (runServerAppT)
import Network.Wai (responseLBS)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.HTTP.Types (status400)


protocolTests :: TestTree
protocolTests =
  testGroup "WebSocket Server"
    [ testCase "Json" $
        let tests :: SymbioteT Json.Value IO ()
            tests = do
              register "Generating Topic" 100 (Proxy :: Proxy (Generating Topic))
              register "Operating Topic" 100 (Proxy :: Proxy (Operating Topic))
              register "First Topic" 100 (Proxy :: Proxy (First Topic))
              register "Second Topic" 100 (Proxy :: Proxy (Second Topic))
              register "Topic" 100 (Proxy :: Proxy Topic)
            runClient client = do
              -- runServer "localhost" 3000 server'
              let server = accept client
              server' <- runServerAppT server
              liftIO $ run 3000 $ logStdoutDev $ websocketsOr defaultConnectionOptions server' $ \_ respond ->
                respond $ responseLBS status400 [] "Not a websocket"
        in  firstPeerWebSocketJson runClient NoDebug tests
    , testCase "ByteString" $
        let tests :: SymbioteT BS.ByteString IO ()
            tests = do
              register "Generating ByteString" 10 (Proxy :: Proxy (Generating BS.ByteString))
              register "Operating ByteString" 10 (Proxy :: Proxy (Operating BS.ByteString))
              register "First ByteString" 10 (Proxy :: Proxy (First BS.ByteString))
              register "Second ByteString" 10 (Proxy :: Proxy (Second BS.ByteString))
              register "Topic" 10 (Proxy :: Proxy Topic)
            runClient client = do
              -- runServer "localhost" 3000 server'
              let server = accept client
              server' <- runServerAppT server
              liftIO $ run 3001 $ logStdoutDev $ websocketsOr defaultConnectionOptions server' $ \_ respond ->
                respond $ responseLBS status400 [] "Not a websocket"
        in  firstPeerWebSocketByteString runClient NoDebug tests
    ]

-- Internal instances
instance SymbioteOperation (Generating s) (Generating s) where
  data Operation (Generating s) = GeneratingId
  perform GeneratingId x = x
instance Arbitrary (Operation (Generating s)) where
  arbitrary = pure GeneratingId
instance Json.ToJSON (Operation (Generating s)) where
  toJSON GeneratingId = Json.String "id"
instance Json.FromJSON (Operation (Generating s)) where
  parseJSON (Json.String s)
    | s == "id" = pure GeneratingId
  parseJSON json = Json.typeMismatch "Operation (Generating s)" json
instance Cereal.Serialize (Operation (Generating s)) where
  put GeneratingId = Cereal.putWord8 0
  get = do
    x <- Cereal.getWord8
    case x of
      0 -> pure GeneratingId
      _ -> fail "Operation (Generating s)"

instance SymbioteOperation (Operating s) (Operating s) where
  data Operation (Operating s) = OperatingId
  perform OperatingId x = x
instance Arbitrary (Operation (Operating s)) where
  arbitrary = pure OperatingId
instance Json.ToJSON (Operation (Operating s)) where
  toJSON OperatingId = Json.String "id"
instance Json.FromJSON (Operation (Operating s)) where
  parseJSON (Json.String s)
    | s == "id" = pure OperatingId
  parseJSON json = Json.typeMismatch "Operation (Operating S)" json
instance Cereal.Serialize (Operation (Operating s)) where
  put OperatingId = Cereal.putWord8 0
  get = do
    x <- Cereal.getWord8
    case x of
      0 -> pure OperatingId
      _ -> fail "Operation (Operating s)"

instance SymbioteOperation (First s) (First s) where
  data Operation (First s) = FirstId
  perform FirstId x = x
instance Arbitrary (Operation (First s)) where
  arbitrary = pure FirstId
instance Json.ToJSON (Operation (First s)) where
  toJSON FirstId = Json.String "id"
instance Json.FromJSON (Operation (First s)) where
  parseJSON (Json.String s)
    | s == "id" = pure FirstId
  parseJSON json = Json.typeMismatch "Operation (First S)" json
instance Cereal.Serialize (Operation (First s)) where
  put FirstId = Cereal.putWord8 0
  get = do
    x <- Cereal.getWord8
    case x of
      0 -> pure FirstId
      _ -> fail "Operation (First s)"

instance SymbioteOperation (Second s) (Second s) where
  data Operation (Second s) = SecondId
  perform SecondId x = x
instance Arbitrary (Operation (Second s)) where
  arbitrary = pure SecondId
instance Json.ToJSON (Operation (Second s)) where
  toJSON SecondId = Json.String "id"
instance Json.FromJSON (Operation (Second s)) where
  parseJSON (Json.String s)
    | s == "id" = pure SecondId
  parseJSON json = Json.typeMismatch "Operation (Second S)" json
instance Cereal.Serialize (Operation (Second s)) where
  put SecondId = Cereal.putWord8 0
  get = do
    x <- Cereal.getWord8
    case x of
      0 -> pure SecondId
      _ -> fail "Operation (Second s)"

instance SymbioteOperation Topic Topic where
  data Operation Topic = TopicId
  perform TopicId x = x
instance Arbitrary (Operation Topic) where
  arbitrary = pure TopicId
instance Json.ToJSON (Operation Topic) where
  toJSON TopicId = Json.String "id"
instance Json.FromJSON (Operation Topic) where
  parseJSON (Json.String s)
    | s == "id" = pure TopicId
  parseJSON json = Json.typeMismatch "Operation Topic" json
instance Cereal.Serialize (Operation Topic) where
  put TopicId = Cereal.putWord8 0
  get = do
    x <- Cereal.getWord8
    case x of
      0 -> pure TopicId
      _ -> fail "Operation Topic"
