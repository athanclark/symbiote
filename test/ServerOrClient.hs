{-# LANGUAGE
    OverloadedStrings
  #-}

module ServerOrClient where

import Test.Tasty.Options (safeReadBool, flagCLParser, IsOption (..))
import Data.Bool (bool)


data ServerOrClient = Server | Client

instance IsOption ServerOrClient where
  defaultValue = Server
  parseValue s = (bool Server Client) <$> safeReadBool s
  optionName = "is-client"
  optionHelp = "Set to 'true' when you want the test suite to behave as a client"
  optionCLParser = flagCLParser (Just 'c') Client
