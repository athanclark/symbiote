{-# Language
    TypeFamilies
  , DeriveGeneric
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module Test.Serialization.Symbiote.Abides where

import Control.Category (Category)
import Test.Abides.Data.Semigroup (associative)
import Test.Serialization.Symbiote.Core (SymbioteOperation (Operation, perform))
import GHC.Generics (Generic)


newtype AbidesSemigroup a = AbidesSemigroup {getAbidesSemigroup :: a}
  deriving (Generic, Eq, Show, Semigroup)

newtype AbidesMonoid a = AbidesMonoid {getAbidesMonoid :: a}
  deriving (Generic, Eq, Show, Semigroup, Monoid)

newtype AbidesEq a = AbidesEq {getAbidesEq :: a}
  deriving (Generic, Eq, Show)

newtype AbidesOrd a = AbidesOrd {getAbidesOrd :: a}
  deriving (Generic, Eq, Show, Ord)

newtype AbidesEnum a = AbidesEnum {getAbidesEnum :: a}
  deriving (Generic, Eq, Show, Enum)

newtype AbidesFunctor f a = AbidesFunctor {getAbidesFunctor :: f a}
  deriving (Generic, Eq, Show, Functor)

newtype AbidesCategory c a b = AbidesCategory {getAbidesCategory :: c a b}
  deriving (Generic, Eq, Show, Category)




instance (Semigroup a, Eq a) => SymbioteOperation (AbidesSemigroup a) Bool where
  data Operation (AbidesSemigroup a)
    = SemigroupAssociative (AbidesSemigroup a) (AbidesSemigroup a)
  perform op x = case op of
    SemigroupAssociative y z -> associative x y z
