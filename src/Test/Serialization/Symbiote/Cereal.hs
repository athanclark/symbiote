{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , UndecidableInstances
  #-}

{-|

Module: Test.Serialization.Symbiote.Cereal
Copyright: (c) 2019 Athan Clark
License: BSD-3-Style
Maintainer: athan.clark@gmail.com
Portability: GHC

-}

module Test.Serialization.Symbiote.Cereal where

import Test.Serialization.Symbiote (SymbioteOperation, Symbiote (..), Operation)
import qualified Data.Serialize as Cereal
import qualified Data.ByteString as BS

instance
  ( Cereal.Serialize a
  , Cereal.Serialize o
  , Cereal.Serialize (Operation a)
  , SymbioteOperation a o
  ) => Symbiote a o BS.ByteString where
  encode = Cereal.encode
  decode x = case Cereal.decode x of
    Left _ -> Nothing
    Right y -> Just y
  encodeOp = Cereal.encode
  decodeOp x = case Cereal.decode x of
    Left _ -> Nothing
    Right y -> Just y
  encodeOut _ = Cereal.encode
  decodeOut _ x = case Cereal.decode x of
    Left _ -> Nothing
    Right y -> Just y
