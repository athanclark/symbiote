{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , UndecidableInstances
  #-}

module Test.Serialization.Symbiote.Cereal.Lazy where

import Test.Serialization.Symbiote (SymbioteOperation, Symbiote (..), Operation)
import qualified Data.Serialize as Cereal
import qualified Data.ByteString.Lazy as LBS

instance
  ( Cereal.Serialize a
  , Cereal.Serialize (Operation a)
  , SymbioteOperation a
  ) => Symbiote a LBS.ByteString where
  encode = Cereal.encodeLazy
  decode x = case Cereal.decodeLazy x of
    Left _ -> Nothing
    Right y -> Just y
  encodeOp = Cereal.encodeLazy
  decodeOp x = case Cereal.decodeLazy x of
    Left _ -> Nothing
    Right y -> Just y
