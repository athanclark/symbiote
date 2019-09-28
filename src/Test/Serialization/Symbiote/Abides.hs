{-# Language
    TypeFamilies
  , DeriveGeneric
  , FlexibleInstances
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module Test.Serialization.Symbiote.Abides where

import Control.Category (Category)
import qualified Test.Abides.Data.Semigroup as Semigroup
import qualified Test.Abides.Data.Monoid as Monoid
import qualified Test.Abides.Data.Eq as Eq
import qualified Test.Abides.Data.Ord as Ord
import qualified Test.Abides.Data.Enum as Enum
import qualified Test.Abides.Control.Category as Category
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
  deriving (Generic, Eq, Ord, Show, Enum)

newtype AbidesCategory c a b = AbidesCategory {getAbidesCategory :: c a b}
  deriving (Generic, Eq, Show, Category)




instance (Semigroup a, Eq a) => SymbioteOperation (AbidesSemigroup a) Bool where
  data Operation (AbidesSemigroup a)
    = SemigroupAssociative (AbidesSemigroup a) (AbidesSemigroup a)
  perform op x = case op of
    SemigroupAssociative y z -> Semigroup.associative x y z

instance (Monoid a, Eq a) => SymbioteOperation (AbidesMonoid a) Bool where
  data Operation (AbidesMonoid a)
    = MonoidAssociative (AbidesMonoid a) (AbidesMonoid a)
    | MonoidLeftIdentity
    | MonoidRightIdentity
  perform op x = case op of
    MonoidAssociative y z -> Semigroup.associative x y z
    MonoidLeftIdentity -> Monoid.leftIdentity x
    MonoidRightIdentity -> Monoid.rightIdentity x

instance (Eq a) => SymbioteOperation (AbidesEq a) Bool where
  data Operation (AbidesEq a)
    = EqReflexive
    | EqSymmetry (AbidesEq a)
    | EqTransitive (AbidesEq a) (AbidesEq a)
    | EqNegation (AbidesEq a)
  perform op x = case op of
    EqReflexive -> Eq.reflexive x
    EqSymmetry y -> Eq.symmetry x y
    EqTransitive y z -> Eq.transitive x y z
    EqNegation y -> Eq.negation x y

instance (Ord a) => SymbioteOperation (AbidesOrd a) Bool where
  data Operation (AbidesOrd a)
    = OrdReflexive
    | OrdAntiSymmetry (AbidesOrd a)
    | OrdTransitive (AbidesOrd a) (AbidesOrd a)
  perform op x = case op of
    OrdReflexive -> Ord.reflexive x
    OrdAntiSymmetry y -> Ord.antisymmetry x y
    OrdTransitive y z -> Ord.transitive x y z

instance (Enum a, Ord a) => SymbioteOperation (AbidesEnum a) Bool where
  data Operation (AbidesEnum a)
    = EnumCompareHom (AbidesEnum a)
    | EnumPredSucc
    | EnumSuccPred
  perform op x = case op of
    EnumCompareHom y -> Enum.compareHom x y
    EnumPredSucc -> Enum.predsucc x
    EnumSuccPred -> Enum.succpred x

instance (Category c, Eq (c a a)) => SymbioteOperation (AbidesCategory c a a) Bool where
  data Operation (AbidesCategory c a a)
    = CategoryIdentity
    | CategoryAssociative (AbidesCategory c a a) (AbidesCategory c a a)
  perform op x = case op of
    CategoryIdentity -> Category.identity x
    CategoryAssociative y z -> Category.associative x y z
