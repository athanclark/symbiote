{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , UndecidableInstances
  #-}

{-|

Module: Test.Serialization.Symbiote.Cereal.Lazy
Copyright: (c) 2019 Athan Clark
License: BSD-3-Style
Maintainer: athan.clark@gmail.com
Portability: GHC

-}

module Test.Serialization.Symbiote.Cereal.Lazy where

import Test.Serialization.Symbiote (SymbioteOperation, Symbiote (..), Operation)
import qualified Data.Serialize as Cereal
import qualified Data.ByteString.Lazy as LBS

instance
  ( Cereal.Serialize a
  , Cereal.Serialize o
  , Cereal.Serialize (Operation a)
  , SymbioteOperation a o
  ) => Symbiote a o LBS.ByteString where
  encode = Cereal.encodeLazy
  decode x = case Cereal.decodeLazy x of
    Left _ -> Nothing
    Right y -> Just y
  encodeOp = Cereal.encodeLazy
  decodeOp x = case Cereal.decodeLazy x of
    Left _ -> Nothing
    Right y -> Just y
  encodeOut _ = Cereal.encodeLazy
  decodeOut _ x = case Cereal.decodeLazy x of
    Left _ -> Nothing
    Right y -> Just y
