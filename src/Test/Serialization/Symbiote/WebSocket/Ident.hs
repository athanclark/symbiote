{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , RecordWildCards
  , NamedFieldPuns
  , OverloadedStrings
  #-}

module Test.Serialization.Symbiote.WebSocket.Ident where

import Data.UUID (UUID, toText, fromText, toWords, fromWords)
import Data.UUID.V4 (nextRandom)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String, Object), object, (.:), (.=))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getWord32be)
import Data.Serialize.Put (putWord32be)
import Data.Hashable (Hashable)
import Test.QuickCheck (Arbitrary (..))
import System.IO.Unsafe (unsafePerformIO)
import Network.WebSockets.Simple (WebSocketsApp (..))

newtype WebSocketIdent = WebSocketIdent {getWebSocketIdent :: UUID}
  deriving (Show, Eq, Ord, Hashable)

newWebSocketIdent :: IO WebSocketIdent
newWebSocketIdent = WebSocketIdent <$> nextRandom

instance ToJSON WebSocketIdent where
  toJSON (WebSocketIdent x) = String (toText x)

instance FromJSON WebSocketIdent where
  parseJSON json = case json of
    String s -> case fromText s of
      Nothing -> fail'
      Just x -> pure (WebSocketIdent x)
    _ -> fail'
    where
      fail' = typeMismatch "WebSocketIdent" json

instance Serialize WebSocketIdent where
  get = WebSocketIdent <$> (fromWords <$> getWord32be <*> getWord32be <*> getWord32be <*> getWord32be)
  put (WebSocketIdent x) =
    let (a,b,c,d) = toWords x
    in  putWord32be a *> putWord32be b *> putWord32be c *> putWord32be d

instance Arbitrary WebSocketIdent where
  arbitrary = pure (unsafePerformIO newWebSocketIdent)


data WithWebSocketIdent a = WithWebSocketIdent
  { webSocketIdent :: WebSocketIdent
  , webSocketValue :: a
  } deriving (Show, Eq)

instance ToJSON a => ToJSON (WithWebSocketIdent a) where
  toJSON WithWebSocketIdent{..} = object ["ident" .= webSocketIdent, "value" .= webSocketValue]

instance FromJSON a => FromJSON (WithWebSocketIdent a) where
  parseJSON json = case json of
    Object o -> WithWebSocketIdent <$> o .: "ident" <*> o .: "value"
    _ -> typeMismatch "WithWebSocketIdent" json

instance Serialize a => Serialize (WithWebSocketIdent a) where
  get = do
    webSocketIdent <- get
    webSocketValue <- get
    pure WithWebSocketIdent{webSocketIdent,webSocketValue}
  put WithWebSocketIdent{..} = do
    put webSocketIdent
    put webSocketValue

instance Arbitrary a => Arbitrary (WithWebSocketIdent a) where
  arbitrary = WithWebSocketIdent <$> arbitrary <*> arbitrary
