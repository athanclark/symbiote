{-# LANGUAGE
    OverloadedStrings
  , MultiParamTypeClasses
  , TypeFamilies
  , FlexibleInstances
  , StandaloneDeriving
  #-}

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase)
import Test.Serialization.Symbiote
  ( SymbioteT, register, firstPeer, secondPeer, SymbioteOperation (..), Symbiote (..), EitherOp
  , First, Second, simpleTest)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (elements, oneof, scale, getSize)
import Test.QuickCheck.Instances ()

import Data.Proxy (Proxy (..))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import Data.ByteString.Lazy (ByteString)


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "All Tests"
  [ simpleTests
  , bytestringTests
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
        unitSuite :: SymbioteT (EitherOp ()) IO ()
        unitSuite = register "Unit" 100 (Proxy :: Proxy ())
        intSuite :: SymbioteT (EitherOp Int) IO ()
        intSuite = register "Int" 100 (Proxy :: Proxy Int)
        doubleSuite :: SymbioteT (EitherOp Double) IO ()
        doubleSuite = register "Double" 100 (Proxy :: Proxy Double)
        listSuite :: SymbioteT (EitherOp [Int]) IO ()
        listSuite = register "List" 100 (Proxy :: Proxy [Int])
    bytestringTests :: TestTree
    bytestringTests = testGroup "ByteString Tests"
      [ testCase "Json over id" (simpleTest jsonSuite)
      ]
      where
        jsonSuite :: SymbioteT ByteString IO ()
        jsonSuite = register "Json" 100 (Proxy :: Proxy Json.Value)

instance SymbioteOperation () where
  data Operation () = UnitId
  perform UnitId () = ()
deriving instance Show (Operation ())
instance Arbitrary (Operation ()) where
  arbitrary = pure UnitId

instance SymbioteOperation Int where
  data Operation Int
    = AddInt Int
    | SubInt Int
    | DivInt Int
    | MulInt Int
    | ModInt Int
  perform op x = case op of
    AddInt y -> x + y
    SubInt y -> x - y
    DivInt y -> if y == 0 then 0 else x `div` y
    MulInt y -> x * y
    ModInt y -> if y == 0 then 0 else x `mod` y
deriving instance Show (Operation Int)
instance Arbitrary (Operation Int) where
  arbitrary = oneof
    [ AddInt <$> arbitrary
    , SubInt <$> arbitrary
    , DivInt <$> arbitrary
    , MulInt <$> arbitrary
    , ModInt <$> arbitrary
    ]

instance SymbioteOperation Double where
  data Operation Double
    = AddDouble Double
    | SubDouble Double
    | DivDouble Double
    | MulDouble Double
    | RecipDouble
  perform op x = case op of
    AddDouble y -> x + y
    SubDouble y -> x - y
    DivDouble y -> if y == 0.0 then 0.0 else x / y
    MulDouble y -> x * y
    RecipDouble -> if x == 0.0 then 0.0 else recip x
deriving instance Show (Operation Double)
instance Arbitrary (Operation Double) where
  arbitrary = oneof
    [ AddDouble <$> arbitrary
    , SubDouble <$> arbitrary
    , DivDouble <$> arbitrary
    , MulDouble <$> arbitrary
    , pure RecipDouble
    ]

instance SymbioteOperation [a] where
  data Operation [a]
    = ReverseList
    | InitList
    | TailList
  perform op x = case op of
    ReverseList -> reverse x
    InitList -> if length x == 0 then [] else init x
    TailList -> if length x == 0 then [] else tail x
deriving instance Show (Operation [a])
instance Arbitrary (Operation [a]) where
  arbitrary = elements [ReverseList, InitList, TailList]

instance SymbioteOperation Json.Value where
  data Operation Json.Value = JsonId
  perform _ x = x
deriving instance Show (Operation Json.Value)
instance Arbitrary (Operation Json.Value) where
  arbitrary = pure JsonId
instance Symbiote Json.Value ByteString where
  encode = Json.encode
  decode = Json.decode
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
