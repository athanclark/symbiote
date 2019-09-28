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
import qualified Test.Abides.Data.Semiring as Semiring
import qualified Test.Abides.Data.Ring as Ring
import qualified Test.Abides.Data.CommutativeRing as CommutativeRing
import qualified Test.Abides.Data.DivisionRing as DivisionRing
import qualified Test.Abides.Data.EuclideanRing as EuclideanRing
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

newtype AbidesSemiring a = AbidesSemiring {getAbidesSemiring :: a}
  deriving (Generic, Eq, Show, Num)

newtype AbidesRing a = AbidesRing {getAbidesRing :: a}
  deriving (Generic, Eq, Show, Num)

newtype AbidesCommutativeRing a = AbidesCommutativeRing {getAbidesCommutativeRing :: a}
  deriving (Generic, Eq, Show, Num)

newtype AbidesDivisionRing a = AbidesDivisionRing {getAbidesDivisionRing :: a}
  deriving (Generic, Eq, Show, Num, Fractional)

newtype AbidesEuclideanRing a = AbidesEuclideanRing {getAbidesEuclideanRing :: a}
  deriving (Generic, Eq, Show, Num)

newtype AbidesField a = AbidesField {getAbidesField :: a}
  deriving (Generic, Eq, Show, Num, Fractional)




instance (Semigroup a, Eq a) => SymbioteOperation (AbidesSemigroup a) Bool where
  data Operation (AbidesSemigroup a)
    = SemigroupAssociative (AbidesSemigroup a) (AbidesSemigroup a)
  perform op x = case op of
    SemigroupAssociative y z -> Semigroup.associative x y z

instance (Monoid a, Eq a) => SymbioteOperation (AbidesMonoid a) Bool where
  data Operation (AbidesMonoid a)
    = MonoidSemigroup (Operation (AbidesSemigroup a))
    | MonoidLeftIdentity
    | MonoidRightIdentity
  perform op x@(AbidesMonoid x') = case op of
    MonoidSemigroup op' -> perform op' (AbidesSemigroup x')
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

instance (Num a, Eq a) => SymbioteOperation (AbidesSemiring a) Bool where
  data Operation (AbidesSemiring a)
    = SemiringCommutativeMonoid (AbidesSemiring a) (AbidesSemiring a)
    | SemiringMonoid (AbidesSemiring a) (AbidesSemiring a)
    | SemiringLeftDistributive (AbidesSemiring a) (AbidesSemiring a)
    | SemiringRightDistributive (AbidesSemiring a) (AbidesSemiring a)
    | SemiringAnnihilation
  perform op x = case op of
    SemiringCommutativeMonoid y z -> Semiring.commutativeMonoid x y z
    SemiringMonoid y z -> Semiring.monoid x y z
    SemiringLeftDistributive y z -> Semiring.leftDistributive x y z
    SemiringRightDistributive y z -> Semiring.rightDistributive x y z
    SemiringAnnihilation -> Semiring.annihilation x

instance (Num a, Eq a) => SymbioteOperation (AbidesRing a) Bool where
  data Operation (AbidesRing a)
    = RingSemiring (Operation (AbidesSemiring a))
    | RingAdditiveInverse
  perform op x@(AbidesRing x') = case op of
    RingSemiring op' -> perform op' (AbidesSemiring x')
    RingAdditiveInverse -> Ring.additiveInverse x

instance (Num a, Eq a) => SymbioteOperation (AbidesCommutativeRing a) Bool where
  data Operation (AbidesCommutativeRing a)
    = CommutativeRingRing (Operation (AbidesRing a))
    | CommutativeRingCommutative (AbidesCommutativeRing a)
  perform op x@(AbidesCommutativeRing x') = case op of
    CommutativeRingRing op' -> perform op' (AbidesRing x')
    CommutativeRingCommutative y -> CommutativeRing.commutative x y

instance (Fractional a, Eq a) => SymbioteOperation (AbidesDivisionRing a) Bool where
  data Operation (AbidesDivisionRing a)
    = DivisionRingRing (Operation (AbidesRing a))
    | DivisionRingInverse
  perform op x@(AbidesDivisionRing x') = case op of
    DivisionRingRing op' -> perform op' (AbidesRing x')
    DivisionRingInverse -> DivisionRing.inverse x

instance (Num a, Eq a) => SymbioteOperation (AbidesEuclideanRing a) Bool where
  data Operation (AbidesEuclideanRing a)
    = EuclideanRingRing (Operation (AbidesRing a))
    | EuclideanRingIntegralDomain (AbidesEuclideanRing a)
  perform op x@(AbidesEuclideanRing x') = case op of
    EuclideanRingRing op' -> perform op' (AbidesRing x')
    EuclideanRingIntegralDomain y -> EuclideanRing.integralDomain x y

instance (Fractional a, Eq a) => SymbioteOperation (AbidesField a) Bool where
  data Operation (AbidesField a)
    = FieldDivisionRing (Operation (AbidesDivisionRing a))
    | FieldEuclideanRing (Operation (AbidesEuclideanRing a))
  perform op (AbidesField x') = case op of
    FieldDivisionRing op' -> perform op' (AbidesDivisionRing x')
    FieldEuclideanRing op' -> perform op' (AbidesEuclideanRing x')
