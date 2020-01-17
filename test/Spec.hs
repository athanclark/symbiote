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

import Test.Tasty (defaultMainWithIngredients, defaultIngredients, includingOptions, testGroup, TestTree)
import Test.Tasty.Options (OptionDescription (..))
import ServerOrClient (ServerOrClient)
import Data.Proxy (Proxy (..))


main :: IO ()
main = defaultMainWithIngredients (includingOptions [serverOrClientArg] : defaultIngredients) tests
  where
    serverOrClientArg = Option (Proxy :: Proxy ServerOrClient)

tests :: TestTree
tests = testGroup "All Tests" $
  [ sanityTests
  , localIsos
  ] ++ protocolTests
