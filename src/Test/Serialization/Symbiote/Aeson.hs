{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , UndecidableInstances
  #-}

{-|

Module: Test.Serialization.Symbiote.Aeson
Copyright: (c) 2019 Athan Clark
License: BSD-3-Style
Maintainer: athan.clark@gmail.com
Portability: GHC

-}

module Test.Serialization.Symbiote.Aeson where

import Test.Serialization.Symbiote (SymbioteOperation, Symbiote (..), Operation)
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json

instance
  ( Json.ToJSON a
  , Json.FromJSON a
  , Json.ToJSON o
  , Json.FromJSON o
  , Json.ToJSON (Operation a)
  , Json.FromJSON (Operation a)
  , SymbioteOperation a o
  ) => Symbiote a o Json.Value where
  encode = Json.toJSON
  decode = Json.parseMaybe Json.parseJSON
  encodeOp = Json.toJSON
  decodeOp = Json.parseMaybe Json.parseJSON
  encodeOut _ = Json.toJSON
  decodeOut _ = Json.parseMaybe Json.parseJSON
