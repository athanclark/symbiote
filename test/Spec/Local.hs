module Spec.Local where

import Spec.Types ()

import qualified Data.Aeson as Json
import qualified Data.Serialize as Cereal
import Data.ByteString (ByteString)
import Data.Proxy (Proxy (..))
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck.Instances ()
import Test.Serialization.Symbiote (First, Second, Generating, Operating, Topic)
import Test.Serialization.Symbiote.Abides
import Test.Serialization.Symbiote.WebSocket.Ident (WebSocketIdent, WithWebSocketIdent)


localIsos :: TestTree
localIsos =
  testGroup "Local Isomorphisms"
    [ testGroup "Json" $
      let go (n,p) = testProperty n (jsonIso p)
      in  [ testGroup "Abides"
            [ go ("AbidesSemigroup [Int]", Proxy :: Proxy (AbidesSemigroup [Int]))
            , go ("AbidesMonoid [Int]", Proxy :: Proxy (AbidesMonoid [Int]))
            , go ("AbidesEq Int", Proxy :: Proxy (AbidesEq Int))
            , go ("AbidesOrd Int", Proxy :: Proxy (AbidesOrd Int))
            , go ("AbidesEnum Int", Proxy :: Proxy (AbidesEnum Int))
            , go ("AbidesSemiring Int", Proxy :: Proxy (AbidesSemiring Int))
            , go ("AbidesRing Int", Proxy :: Proxy (AbidesRing Int))
            , go ("AbidesCommutativeRing Int", Proxy :: Proxy (AbidesCommutativeRing Int))
            , go ("AbidesDivisionRing Int", Proxy :: Proxy (AbidesDivisionRing Int))
            , go ("AbidesEuclideanRing Int", Proxy :: Proxy (AbidesEuclideanRing Int))
            , go ("AbidesField Int", Proxy :: Proxy (AbidesField Int))
            ]
          , testGroup "Symbiote"
            [ go ("Generating Int", Proxy :: Proxy (Generating Int))
            , go ("Operating Int", Proxy :: Proxy (Operating Int))
            , go ("First Int", Proxy :: Proxy (First Int))
            , go ("Second Int", Proxy :: Proxy (Second Int))
            , go ("Topic", Proxy :: Proxy Topic)
            , go ("WebSocketIdent", Proxy :: Proxy WebSocketIdent)
            , go ("WithWebSocketIdent", Proxy :: Proxy (WithWebSocketIdent Int))
            ]
          ]
    , testGroup "Cereal" $
      let go (n,p) = testProperty n (cerealIso p)
      in  [ testGroup "Abides"
            [ go ("AbidesSemigroup [Int]", Proxy :: Proxy (AbidesSemigroup [Int]))
            , go ("AbidesMonoid [Int]", Proxy :: Proxy (AbidesMonoid [Int]))
            , go ("AbidesEq Int", Proxy :: Proxy (AbidesEq Int))
            , go ("AbidesOrd Int", Proxy :: Proxy (AbidesOrd Int))
            , go ("AbidesEnum Int", Proxy :: Proxy (AbidesEnum Int))
            , go ("AbidesSemiring Int", Proxy :: Proxy (AbidesSemiring Int))
            , go ("AbidesRing Int", Proxy :: Proxy (AbidesRing Int))
            , go ("AbidesCommutativeRing Int", Proxy :: Proxy (AbidesCommutativeRing Int))
            , go ("AbidesDivisionRing Int", Proxy :: Proxy (AbidesDivisionRing Int))
            , go ("AbidesEuclideanRing Int", Proxy :: Proxy (AbidesEuclideanRing Int))
            , go ("AbidesField Int", Proxy :: Proxy (AbidesField Int))
            ]
          , testGroup "Symbiote"
            [ go ("Generating ByteString", Proxy :: Proxy (Generating ByteString))
            , go ("Operating ByteString", Proxy :: Proxy (Operating ByteString))
            , go ("First ByteString", Proxy :: Proxy (First ByteString))
            , go ("Second ByteString", Proxy :: Proxy (Second ByteString))
            , go ("Topic", Proxy :: Proxy Topic)
            , go ("WebSocketIdent", Proxy :: Proxy WebSocketIdent)
            , go ("WithWebSocketIdent", Proxy :: Proxy (WithWebSocketIdent Int))
            ]
          ]
    ]


jsonIso :: Json.ToJSON a => Json.FromJSON a => Eq a => Proxy a -> a -> Bool
jsonIso Proxy x = Json.decode (Json.encode x) == Just x


cerealIso :: Cereal.Serialize a => Eq a => Proxy a -> a -> Bool
cerealIso Proxy x = Cereal.decode (Cereal.encode x) == Right x
