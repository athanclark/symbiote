{-# LANGUAGE
    TypeFamilies
  , DeriveGeneric
  , FlexibleContexts
  , OverloadedStrings
  , FlexibleInstances
  , StandaloneDeriving
  , UndecidableInstances
  , MultiParamTypeClasses
  #-}

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.Serialization.Symbiote
  ( SymbioteT, register, firstPeer, secondPeer, SymbioteOperation (..), Symbiote (..), SimpleSerialization
  , First, Second, Generating, Operating, Topic, simpleTest, defaultSuccess, defaultFailure, defaultProgress)
import Test.Serialization.Symbiote.Cereal ()
import Test.Serialization.Symbiote.Aeson ()
import Test.Serialization.Symbiote.Abides
import Test.Serialization.Symbiote.WebSocket (firstPeerWebSocketJson, Debug (..))
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (elements, oneof, scale, getSize)
import Test.QuickCheck.Instances ()

import Data.Proxy (Proxy (..))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Serialize as Cereal
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Network.WebSockets.Connection (defaultConnectionOptions)
import Network.WebSockets.Simple (accept)
import Network.WebSockets.Trans (runServerAppT)
import Network.Wai (responseLBS)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.HTTP.Types (status400)


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "All Tests"
  [ testGroup "Symbiote Sanity Checks"
    [ simpleTests
    , bytestringTests
    , jsonTests
    ]
  , testGroup "Local Isomorphisms"
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
            [ go ("Generating Int", Proxy :: Proxy (Generating Int))
            , go ("Operating Int", Proxy :: Proxy (Operating Int))
            , go ("First Int", Proxy :: Proxy (First Int))
            , go ("Second Int", Proxy :: Proxy (Second Int))
            , go ("Topic", Proxy :: Proxy Topic)
            ]
          ]
    , testGroup "WebSocket Server" $
      let runClient client = do
            let server = accept client
            -- runServer "localhost" 3000 server'
            let server = accept client
            server' <- runServerAppT server
            liftIO $ run 3000 $ logStdoutDev $ websocketsOr defaultConnectionOptions server' $ \_ respond ->
              respond $ responseLBS status400 [] "Not a websocket"
      in  [ testCase "Json" $
              let tests :: SymbioteT Json.Value IO ()
                  tests = do
                    register "Generating Topic" 100 (Proxy :: Proxy (Generating Topic))
                    register "Operating Topic" 100 (Proxy :: Proxy (Operating Topic))
                    register "First Topic" 100 (Proxy :: Proxy (First Topic))
                    register "Second Topic" 100 (Proxy :: Proxy (Second Topic))
                    register "Topic" 100 (Proxy :: Proxy Topic)
              in  firstPeerWebSocketJson runClient NoDebug tests
          ]
    ]
  ]
  where
    simpleTests :: TestTree
    simpleTests = testGroup "Simple Tests"
      [ testCase "Unit over id" (simpleTest unitSuite)
      , testCase "Int over various" (simpleTest intSuite)
      , testCase "Double over various" (simpleTest doubleSuite)
      , testCase "List over various" (simpleTest listSuite)
      ]
      where
        unitSuite :: SymbioteT (SimpleSerialization () ()) IO ()
        unitSuite = register "Unit" 100 (Proxy :: Proxy ())
        intSuite :: SymbioteT (SimpleSerialization Int Bool) IO ()
        intSuite = register "Int" 100 (Proxy :: Proxy Int)
        doubleSuite :: SymbioteT (SimpleSerialization Double Bool) IO ()
        doubleSuite = register "Double" 100 (Proxy :: Proxy Double)
        listSuite :: SymbioteT (SimpleSerialization [Int] (Either Bool [Int])) IO ()
        listSuite = register "List" 100 (Proxy :: Proxy [Int])
    bytestringTests :: TestTree
    bytestringTests = testGroup "ByteString Tests"
      [ testCase "Json over id" (simpleTest jsonSuiteByteString)
      , testCase "Int over various" (simpleTest intSuiteByteString)
      , testCase "Double over various" (simpleTest doubleSuiteByteString)
      , testCase "List over various" (simpleTest listSuiteByteString)
      ]
      where
        jsonSuiteByteString :: SymbioteT LBS.ByteString IO ()
        jsonSuiteByteString = register "Json" 100 (Proxy :: Proxy Json.Value)
        intSuiteByteString :: SymbioteT BS.ByteString IO ()
        intSuiteByteString = register "Int" 100 (Proxy :: Proxy Int)
        doubleSuiteByteString :: SymbioteT BS.ByteString IO ()
        doubleSuiteByteString = register "Double" 100 (Proxy :: Proxy Double)
        listSuiteByteString :: SymbioteT BS.ByteString IO ()
        listSuiteByteString = register "List" 100 (Proxy :: Proxy [Int])
    jsonTests :: TestTree
    jsonTests = testGroup "Json Tests"
      [ testCase "Int over various" (simpleTest intSuiteJson)
      , testCase "Double over various" (simpleTest doubleSuiteJson)
      , testCase "List over various" (simpleTest listSuiteJson)
      ]
      where
        intSuiteJson :: SymbioteT Json.Value IO ()
        intSuiteJson = register "Int" 100 (Proxy :: Proxy Int)
        doubleSuiteJson :: SymbioteT Json.Value IO ()
        doubleSuiteJson = register "Double" 100 (Proxy :: Proxy Double)
        listSuiteJson :: SymbioteT Json.Value IO ()
        listSuiteJson = register "List" 100 (Proxy :: Proxy [Int])

instance SymbioteOperation () () where
  data Operation () = UnitId
  perform UnitId () = ()
deriving instance Show (Operation ())
deriving instance Generic (Operation ())
instance Arbitrary (Operation ()) where
  arbitrary = pure UnitId

instance SymbioteOperation Int Bool where
  data Operation Int
    = IntCommutativeRing (Operation (AbidesCommutativeRing Int))
  perform op x = case op of
    IntCommutativeRing op' -> perform op' (AbidesCommutativeRing x)
deriving instance Show (Operation Int)
deriving instance Generic (Operation Int)
instance Cereal.Serialize (Operation Int)
instance Json.ToJSON (Operation Int)
instance Json.FromJSON (Operation Int)
instance Arbitrary (Operation Int) where
  arbitrary = IntCommutativeRing <$> arbitrary


instance SymbioteOperation Double Bool where
  data Operation Double
    = DoubleField (Operation (AbidesField Double))
  perform op x = case op of
    DoubleField op' -> perform op' (AbidesField x)
deriving instance Show (Operation Double)
deriving instance Generic (Operation Double)
instance Cereal.Serialize (Operation Double)
instance Json.ToJSON (Operation Double)
instance Json.FromJSON (Operation Double)
instance Arbitrary (Operation Double) where
  arbitrary = DoubleField <$> arbitrary

instance Eq a => SymbioteOperation [a] (Either Bool [a]) where
  data Operation [a]
    = ListMonoid (Operation (AbidesMonoid [a]))
    | ReverseList
    | InitList
    | TailList
  perform op x = case op of
    ListMonoid op' -> Left (perform op' (AbidesMonoid x))
    ReverseList -> Right (reverse x)
    InitList -> Right $ if null x then [] else init x
    TailList -> Right $ if null x then [] else tail x
deriving instance Show a => Show (Operation [a])
deriving instance Generic (Operation [a])
instance Cereal.Serialize a => Cereal.Serialize (Operation [a])
instance Json.ToJSON a => Json.ToJSON (Operation [a])
instance Json.FromJSON a => Json.FromJSON (Operation [a])
instance Arbitrary a => Arbitrary (Operation [a]) where
  arbitrary = oneof
    [ pure ReverseList
    , pure InitList
    , pure TailList
    , ListMonoid <$> arbitrary
    ]

instance SymbioteOperation Json.Value Json.Value where
  data Operation Json.Value = JsonId
  perform _ x = x
deriving instance Show (Operation Json.Value)
deriving instance Generic (Operation Json.Value)
instance Arbitrary (Operation Json.Value) where
  arbitrary = pure JsonId
instance Symbiote Json.Value Json.Value LBS.ByteString where
  encode = Json.encode
  decode = Json.decode
  encodeOut _ = Json.encode
  decodeOut _ = Json.decode
  encodeOp _ = "id"
  decodeOp x | x == "id" = Just JsonId
             | otherwise = Nothing
instance Arbitrary Json.Value where
  arbitrary = do
    s <- getSize
    if s <= 1
      then oneof
            [ pure Json.Null
            , Json.Bool <$> arbitrary
            , Json.Number <$> arbitrary
            ]
      else oneof
            [ Json.String <$> scale (`div` 2) arbitrary
            , Json.Array <$> scale (`div` 2) arbitrary
            , Json.Object <$> scale (`div` 2) arbitrary
            ]



jsonIso :: Json.ToJSON a => Json.FromJSON a => Eq a => Proxy a -> a -> Bool
jsonIso Proxy x = Json.decode (Json.encode x) == Just x


cerealIso :: Cereal.Serialize a => Eq a => Proxy a -> a -> Bool
cerealIso Proxy x = Cereal.decode (Cereal.encode x) == Right x



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
