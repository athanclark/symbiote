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
import Test.QuickCheck.Gen (elements)

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
      , testCase "List over various" (simpleTest listSuite)
      ]
      where
        unitSuite :: SymbioteT (EitherOp ()) IO ()
        unitSuite = register "Unit" 100 (Proxy :: Proxy ())
        intSuite :: SymbioteT (EitherOp Int) IO ()
        intSuite = register "Int" 100 (Proxy :: Proxy Int)
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
    = Add5Int
    | Sub5Int
    | Div2Int
    | Mul2Int
    | Mod12Int
  perform op x = case op of
    Add5Int -> x + 5
    Sub5Int -> x - 5
    Div2Int -> x `div` 2
    Mul2Int -> x * 2
    Mod12Int -> x `mod` 12
deriving instance Show (Operation Int)
instance Arbitrary (Operation Int) where
  arbitrary = elements [Add5Int, Sub5Int, Div2Int, Mul2Int, Mod12Int]

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
