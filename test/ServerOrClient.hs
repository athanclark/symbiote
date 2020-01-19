{-# LANGUAGE
    OverloadedStrings
  #-}

module ServerOrClient where

import Test.Tasty.Options (safeReadBool, flagCLParser, IsOption (..))
import Test.Serialization.Symbiote.Debug (Network (..))
import Data.Bool (bool)


data ServerOrClient = Server | Client

instance IsOption ServerOrClient where
  defaultValue = Server
  parseValue s = (bool Server Client) <$> safeReadBool s
  optionName = "is-client"
  optionHelp = "Set to 'true' when you want the test suite to behave as a client"
  optionCLParser = flagCLParser (Just 'c') Client


instance IsOption Network where
  defaultValue = Private
  parseValue s = (bool Private Public) <$> safeReadBool s
  optionName = "is-public"
  optionHelp = "Set to 'true' when you want the test suite to behave on a public channel"
