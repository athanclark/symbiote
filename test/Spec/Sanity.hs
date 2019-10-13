{-# LANGUAGE
    OverloadedStrings
  #-}

module Spec.Sanity where

import Spec.Types ()

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Json
import Data.Proxy (Proxy (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Serialization.Symbiote (SymbioteT, SimpleSerialization, simpleTest, register)
import Test.Serialization.Symbiote.Cereal ()
import Test.Serialization.Symbiote.Aeson ()



sanityTests :: TestTree
sanityTests =
  testGroup "Symbiote Sanity Checks"
    [ simpleTests
    , bytestringTests
    , jsonTests
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
