{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , UndecidableInstances
  #-}

module Test.Serialization.Symbiote.Aeson where

import Test.Serialization.Symbiote (SymbioteOperation, Symbiote (..), Operation)
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json

instance
  ( Json.ToJSON a
  , Json.FromJSON a
  , Json.ToJSON (Operation a)
  , Json.FromJSON (Operation a)
  , SymbioteOperation a
  ) => Symbiote a Json.Value where
  encode = Json.toJSON
  decode = Json.parseMaybe Json.parseJSON
  encodeOp = Json.toJSON
  decodeOp = Json.parseMaybe Json.parseJSON
