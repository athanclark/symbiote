{-# LANGUAGE
    StandaloneDeriving
  , MultiParamTypeClasses
  , TypeFamilies
  , FlexibleInstances
  , DeriveGeneric
  , OverloadedStrings
  #-}

module Spec.Types where

import qualified Data.Serialize as Cereal
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as LBS
import Test.Serialization.Symbiote (SymbioteOperation (..), Symbiote (..))
import Test.Serialization.Symbiote.Abides
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof, scale, getSize)
import GHC.Generics (Generic)


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

