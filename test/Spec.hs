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
import Test.Serialization.Symbiote.Debug (Network)

import Test.Tasty (defaultMainWithIngredients, defaultIngredients, includingOptions, testGroup, TestTree)
import Test.Tasty.Options (OptionDescription (..))
import ServerOrClient (ServerOrClient)
import Data.Proxy (Proxy (..))


main :: IO ()
main =
  defaultMainWithIngredients
    (includingOptions [serverOrClientArg, networkArg] : defaultIngredients)
    tests
  where
    serverOrClientArg = Option (Proxy :: Proxy ServerOrClient)
    networkArg = Option (Proxy :: Proxy Network)

tests :: TestTree
tests = testGroup "All Tests" $
  [ sanityTests
  , localIsos
  ] ++ protocolTests
