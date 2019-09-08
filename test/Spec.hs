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
import Test.QuickCheck.Gen (elements, oneof)

import Data.Proxy (Proxy (..))


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "All Tests"
  [ simpleTests
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
