{-# Language
    TypeFamilies
  , DeriveGeneric
  , OverloadedStrings
  , FlexibleInstances
  , StandaloneDeriving
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module Test.Serialization.Symbiote.Abides where

import Data.Aeson (ToJSON (..), FromJSON (..), object, (.=), (.:), Value (Object, String))
import Data.Aeson.Types (typeMismatch)
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
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof, elements)
import GHC.Generics (Generic)


newtype AbidesSemigroup a = AbidesSemigroup {getAbidesSemigroup :: a}
  deriving (Generic, Eq, Show, Semigroup, Arbitrary, ToJSON, FromJSON)

newtype AbidesMonoid a = AbidesMonoid {getAbidesMonoid :: a}
  deriving (Generic, Eq, Show, Semigroup, Monoid, Arbitrary, ToJSON, FromJSON)

newtype AbidesEq a = AbidesEq {getAbidesEq :: a}
  deriving (Generic, Eq, Show, Arbitrary, ToJSON, FromJSON)

newtype AbidesOrd a = AbidesOrd {getAbidesOrd :: a}
  deriving (Generic, Eq, Show, Ord, Arbitrary, ToJSON, FromJSON)

newtype AbidesEnum a = AbidesEnum {getAbidesEnum :: a}
  deriving (Generic, Eq, Ord, Show, Enum, Arbitrary, ToJSON, FromJSON)

newtype AbidesCategory c a b = AbidesCategory {getAbidesCategory :: c a b}
  deriving (Generic, Eq, Show, Category, Arbitrary, ToJSON, FromJSON)

newtype AbidesSemiring a = AbidesSemiring {getAbidesSemiring :: a}
  deriving (Generic, Eq, Show, Num, Arbitrary, ToJSON, FromJSON)

newtype AbidesRing a = AbidesRing {getAbidesRing :: a}
  deriving (Generic, Eq, Show, Num, Arbitrary, ToJSON, FromJSON)

newtype AbidesCommutativeRing a = AbidesCommutativeRing {getAbidesCommutativeRing :: a}
  deriving (Generic, Eq, Show, Num, Arbitrary, ToJSON, FromJSON)

newtype AbidesDivisionRing a = AbidesDivisionRing {getAbidesDivisionRing :: a}
  deriving (Generic, Eq, Show, Num, Fractional, Arbitrary, ToJSON, FromJSON)

newtype AbidesEuclideanRing a = AbidesEuclideanRing {getAbidesEuclideanRing :: a}
  deriving (Generic, Eq, Show, Num, Arbitrary, ToJSON, FromJSON)

newtype AbidesField a = AbidesField {getAbidesField :: a}
  deriving (Generic, Eq, Show, Num, Fractional, Arbitrary, ToJSON, FromJSON)




instance (Semigroup a, Eq a) => SymbioteOperation (AbidesSemigroup a) Bool where
  data Operation (AbidesSemigroup a)
    = SemigroupAssociative (AbidesSemigroup a) (AbidesSemigroup a)
  perform op x = case op of
    SemigroupAssociative y z -> Semigroup.associative x y z
deriving instance Generic (Operation (AbidesSemigroup a))
deriving instance Show a => Show (Operation (AbidesSemigroup a))
instance Arbitrary a => Arbitrary (Operation (AbidesSemigroup a)) where
  arbitrary = SemigroupAssociative <$> arbitrary <*> arbitrary
instance ToJSON a => ToJSON (Operation (AbidesSemigroup a)) where
  toJSON op = case op of
    SemigroupAssociative y z -> object ["y" .= y, "z" .= z]
instance FromJSON a => FromJSON (Operation (AbidesSemigroup a)) where
  parseJSON (Object o) = SemigroupAssociative <$> o .: "y" <*> o .: "z"
  parseJSON x = typeMismatch "Operation (AbidesSemigroup a)" x

instance (Monoid a, Eq a) => SymbioteOperation (AbidesMonoid a) Bool where
  data Operation (AbidesMonoid a)
    = MonoidSemigroup (Operation (AbidesSemigroup a))
    | MonoidLeftIdentity
    | MonoidRightIdentity
  perform op x@(AbidesMonoid x') = case op of
    MonoidSemigroup op' -> perform op' (AbidesSemigroup x')
    MonoidLeftIdentity -> Monoid.leftIdentity x
    MonoidRightIdentity -> Monoid.rightIdentity x
deriving instance Generic (Operation (AbidesMonoid a))
deriving instance Show a => Show (Operation (AbidesMonoid a))
instance Arbitrary a => Arbitrary (Operation (AbidesMonoid a)) where
  arbitrary = oneof
    [ MonoidSemigroup <$> arbitrary
    , pure MonoidLeftIdentity
    , pure MonoidRightIdentity
    ]
instance ToJSON a => ToJSON (Operation (AbidesMonoid a)) where
  toJSON op = case op of
    MonoidSemigroup op' -> object ["semigroup" .= op']
    MonoidLeftIdentity -> String "leftIdentity"
    MonoidRightIdentity -> String "rightIdentity"
instance FromJSON a => FromJSON (Operation (AbidesMonoid a)) where
  parseJSON (Object o) = MonoidSemigroup <$> o .: "semigroup"
  parseJSON x@(String s)
    | s == "leftIdentity" = pure MonoidLeftIdentity
    | s == "rightIdentity" = pure MonoidRightIdentity
    | otherwise = typeMismatch "Operation (AbidesMonoid a)" x
  parseJSON x = typeMismatch "Operation (AbidesMonoid a)" x

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
deriving instance Generic (Operation (AbidesEq a))
deriving instance Show a => Show (Operation (AbidesEq a))
instance Arbitrary a => Arbitrary (Operation (AbidesEq a)) where
  arbitrary = oneof
    [ EqSymmetry <$> arbitrary
    , pure EqReflexive
    , EqTransitive <$> arbitrary <*> arbitrary
    , EqNegation <$> arbitrary
    ]

instance (Ord a) => SymbioteOperation (AbidesOrd a) Bool where
  data Operation (AbidesOrd a)
    = OrdReflexive
    | OrdAntiSymmetry (AbidesOrd a)
    | OrdTransitive (AbidesOrd a) (AbidesOrd a)
  perform op x = case op of
    OrdReflexive -> Ord.reflexive x
    OrdAntiSymmetry y -> Ord.antisymmetry x y
    OrdTransitive y z -> Ord.transitive x y z
deriving instance Generic (Operation (AbidesOrd a))
deriving instance Show a => Show (Operation (AbidesOrd a))
instance Arbitrary a => Arbitrary (Operation (AbidesOrd a)) where
  arbitrary = oneof
    [ OrdAntiSymmetry <$> arbitrary
    , pure OrdReflexive
    , OrdTransitive <$> arbitrary <*> arbitrary
    ]

instance (Enum a, Ord a) => SymbioteOperation (AbidesEnum a) Bool where
  data Operation (AbidesEnum a)
    = EnumCompareHom (AbidesEnum a)
    | EnumPredSucc
    | EnumSuccPred
  perform op x = case op of
    EnumCompareHom y -> Enum.compareHom x y
    EnumPredSucc -> Enum.predsucc x
    EnumSuccPred -> Enum.succpred x
deriving instance Generic (Operation (AbidesEnum a))
deriving instance Show a => Show (Operation (AbidesEnum a))
instance Arbitrary a => Arbitrary (Operation (AbidesEnum a)) where
  arbitrary = oneof
    [ EnumCompareHom <$> arbitrary
    , pure EnumPredSucc
    , pure EnumSuccPred
    ]

-- | Instances are monomorphic @c a a@ due to infinite types.
instance (Category c, Eq (c a a)) => SymbioteOperation (AbidesCategory c a a) Bool where
  data Operation (AbidesCategory c a a)
    = CategoryIdentity
    | CategoryAssociative (AbidesCategory c a a) (AbidesCategory c a a)
  perform op x = case op of
    CategoryIdentity -> Category.identity x
    CategoryAssociative y z -> Category.associative x y z
deriving instance Generic (Operation (AbidesCategory c a a))
deriving instance Show (c a a) => Show (Operation (AbidesCategory c a a))
instance Arbitrary (c a a) => Arbitrary (Operation (AbidesCategory c a a)) where
  arbitrary = oneof
    [ CategoryAssociative <$> arbitrary <*> arbitrary
    , pure CategoryIdentity
    ]

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
deriving instance Generic (Operation (AbidesSemiring a))
deriving instance Show a => Show (Operation (AbidesSemiring a))
instance Arbitrary a => Arbitrary (Operation (AbidesSemiring a)) where
  arbitrary = oneof
    [ SemiringCommutativeMonoid <$> arbitrary <*> arbitrary
    , SemiringMonoid <$> arbitrary <*> arbitrary
    , SemiringLeftDistributive <$> arbitrary <*> arbitrary
    , SemiringRightDistributive <$> arbitrary <*> arbitrary
    , pure SemiringAnnihilation
    ]

instance (Num a, Eq a) => SymbioteOperation (AbidesRing a) Bool where
  data Operation (AbidesRing a)
    = RingSemiring (Operation (AbidesSemiring a))
    | RingAdditiveInverse
  perform op x@(AbidesRing x') = case op of
    RingSemiring op' -> perform op' (AbidesSemiring x')
    RingAdditiveInverse -> Ring.additiveInverse x
deriving instance Generic (Operation (AbidesRing a))
deriving instance Show a => Show (Operation (AbidesRing a))
instance Arbitrary a => Arbitrary (Operation (AbidesRing a)) where
  arbitrary = oneof
    [ RingSemiring <$> arbitrary
    , pure RingAdditiveInverse
    ]

instance (Num a, Eq a) => SymbioteOperation (AbidesCommutativeRing a) Bool where
  data Operation (AbidesCommutativeRing a)
    = CommutativeRingRing (Operation (AbidesRing a))
    | CommutativeRingCommutative (AbidesCommutativeRing a)
  perform op x@(AbidesCommutativeRing x') = case op of
    CommutativeRingRing op' -> perform op' (AbidesRing x')
    CommutativeRingCommutative y -> CommutativeRing.commutative x y
deriving instance Generic (Operation (AbidesCommutativeRing a))
deriving instance Show a => Show (Operation (AbidesCommutativeRing a))
instance Arbitrary a => Arbitrary (Operation (AbidesCommutativeRing a)) where
  arbitrary = oneof
    [ CommutativeRingRing <$> arbitrary
    , CommutativeRingCommutative <$> arbitrary
    ]

instance (Fractional a, Eq a) => SymbioteOperation (AbidesDivisionRing a) Bool where
  data Operation (AbidesDivisionRing a)
    = DivisionRingRing (Operation (AbidesRing a))
    | DivisionRingInverse
  perform op x@(AbidesDivisionRing x') = case op of
    DivisionRingRing op' -> perform op' (AbidesRing x')
    DivisionRingInverse -> DivisionRing.inverse x
deriving instance Generic (Operation (AbidesDivisionRing a))
deriving instance Show a => Show (Operation (AbidesDivisionRing a))
instance Arbitrary a => Arbitrary (Operation (AbidesDivisionRing a)) where
  arbitrary = oneof
    [ DivisionRingRing <$> arbitrary
    , pure DivisionRingInverse
    ]

instance (Num a, Eq a) => SymbioteOperation (AbidesEuclideanRing a) Bool where
  data Operation (AbidesEuclideanRing a)
    = EuclideanRingRing (Operation (AbidesRing a))
    | EuclideanRingIntegralDomain (AbidesEuclideanRing a)
  perform op x@(AbidesEuclideanRing x') = case op of
    EuclideanRingRing op' -> perform op' (AbidesRing x')
    EuclideanRingIntegralDomain y -> EuclideanRing.integralDomain x y
deriving instance Generic (Operation (AbidesEuclideanRing a))
deriving instance Show a => Show (Operation (AbidesEuclideanRing a))
instance Arbitrary a => Arbitrary (Operation (AbidesEuclideanRing a)) where
  arbitrary = oneof
    [ EuclideanRingRing <$> arbitrary
    , EuclideanRingIntegralDomain <$> arbitrary
    ]

instance (Fractional a, Eq a) => SymbioteOperation (AbidesField a) Bool where
  data Operation (AbidesField a)
    = FieldDivisionRing (Operation (AbidesDivisionRing a))
    | FieldEuclideanRing (Operation (AbidesEuclideanRing a))
  perform op (AbidesField x') = case op of
    FieldDivisionRing op' -> perform op' (AbidesDivisionRing x')
    FieldEuclideanRing op' -> perform op' (AbidesEuclideanRing x')
deriving instance Generic (Operation (AbidesField a))
deriving instance Show a => Show (Operation (AbidesField a))
instance Arbitrary a => Arbitrary (Operation (AbidesField a)) where
  arbitrary = oneof
    [ FieldDivisionRing <$> arbitrary
    , FieldEuclideanRing <$> arbitrary
    ]
