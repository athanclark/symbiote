{-# LANGUAGE
    TypeFamilies
  , FlexibleContexts
  , OverloadedStrings
  , FlexibleInstances
  , UndecidableInstances
  , MultiParamTypeClasses
  #-}

import Spec.Sanity (sanityTests)
import Spec.Local (localIsos)
import Spec.Protocol (protocolTests)

import Test.Tasty (defaultMain, testGroup, TestTree)



main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "All Tests" $
  [ sanityTests
  , localIsos
  ] ++ protocolTests
