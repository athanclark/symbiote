{-# Language
    TypeFamilies
  , DeriveGeneric
  , OverloadedStrings
  , FlexibleInstances
  , StandaloneDeriving
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

{-|

This module provides newtypes for ensuring consistent functionality with respect to various class laws:
Monoids, SemiRing, etc are all included via the abides library. Note: This only verifies the /consistency/
of behavior between platforms - if both platforms are broken (return @False@) /consistently/, the tests
will pass. Prevent this by implementing a local test suite with QuickCheck, and use the abides properties
directly.

-}

module Test.Serialization.Symbiote.Abides where

import Data.Aeson (ToJSON (..), FromJSON (..), object, (.=), (.:), Value (Object, String))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (put,get))
import Data.Serialize.Put (putWord8)
import Data.Serialize.Get (getWord8)
import Control.Applicative ((<|>))
import qualified Test.Abides.Data.Semigroup as Semigroup
import qualified Test.Abides.Data.Monoid as Monoid
import qualified Test.Abides.Data.Eq as Eq
import qualified Test.Abides.Data.Ord as Ord
import qualified Test.Abides.Data.Enum as Enum
import qualified Test.Abides.Data.Semiring as Semiring
import qualified Test.Abides.Data.Ring as Ring
import qualified Test.Abides.Data.CommutativeRing as CommutativeRing
import qualified Test.Abides.Data.DivisionRing as DivisionRing
import qualified Test.Abides.Data.EuclideanRing as EuclideanRing
import Test.Serialization.Symbiote.Core (SymbioteOperation (Operation, perform))
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)
import GHC.Generics (Generic)


newtype AbidesSemigroup a = AbidesSemigroup {getAbidesSemigroup :: a}
  deriving (Generic, Eq, Show, Semigroup, Arbitrary, ToJSON, FromJSON, Serialize)

newtype AbidesMonoid a = AbidesMonoid {getAbidesMonoid :: a}
  deriving (Generic, Eq, Show, Semigroup, Monoid, Arbitrary, ToJSON, FromJSON, Serialize)

newtype AbidesEq a = AbidesEq {getAbidesEq :: a}
  deriving (Generic, Eq, Show, Arbitrary, ToJSON, FromJSON, Serialize)

newtype AbidesOrd a = AbidesOrd {getAbidesOrd :: a}
  deriving (Generic, Eq, Show, Ord, Arbitrary, ToJSON, FromJSON, Serialize)

newtype AbidesEnum a = AbidesEnum {getAbidesEnum :: a}
  deriving (Generic, Eq, Ord, Show, Enum, Arbitrary, ToJSON, FromJSON, Serialize)

newtype AbidesSemiring a = AbidesSemiring {getAbidesSemiring :: a}
  deriving (Generic, Eq, Show, Num, Arbitrary, ToJSON, FromJSON, Serialize)

newtype AbidesRing a = AbidesRing {getAbidesRing :: a}
  deriving (Generic, Eq, Show, Num, Arbitrary, ToJSON, FromJSON, Serialize)

newtype AbidesCommutativeRing a = AbidesCommutativeRing {getAbidesCommutativeRing :: a}
  deriving (Generic, Eq, Show, Num, Arbitrary, ToJSON, FromJSON, Serialize)

newtype AbidesDivisionRing a = AbidesDivisionRing {getAbidesDivisionRing :: a}
  deriving (Generic, Eq, Show, Num, Fractional, Arbitrary, ToJSON, FromJSON, Serialize)

newtype AbidesEuclideanRing a = AbidesEuclideanRing {getAbidesEuclideanRing :: a}
  deriving (Generic, Eq, Show, Num, Arbitrary, ToJSON, FromJSON, Serialize)

newtype AbidesField a = AbidesField {getAbidesField :: a}
  deriving (Generic, Eq, Show, Num, Fractional, Arbitrary, ToJSON, FromJSON, Serialize)




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
    SemigroupAssociative y z -> object ["associative" .= object ["y" .= y, "z" .= z]]
instance FromJSON a => FromJSON (Operation (AbidesSemigroup a)) where
  parseJSON (Object o) = do
    o' <- o .: "associative"
    SemigroupAssociative <$> o' .: "y" <*> o' .: "z"
  parseJSON x = typeMismatch "Operation (AbidesSemigroup a)" x
instance Serialize a => Serialize (Operation (AbidesSemigroup a)) where
  put op = case op of
    SemigroupAssociative y z -> put y *> put z
  get = SemigroupAssociative <$> get <*> get

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
instance Serialize a => Serialize (Operation (AbidesMonoid a)) where
  put op = case op of
    MonoidSemigroup op' -> putWord8 0 *> put op'
    MonoidLeftIdentity -> putWord8 1
    MonoidRightIdentity -> putWord8 2
  get = do
    x <- getWord8
    case x of
      0 -> MonoidSemigroup <$> get
      1 -> pure MonoidLeftIdentity
      2 -> pure MonoidRightIdentity
      _ -> fail "Operation (AbidesMonoid a)"

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
instance ToJSON a => ToJSON (Operation (AbidesEq a)) where
  toJSON op = case op of
    EqSymmetry y -> object ["symmetry" .= y]
    EqReflexive -> String "reflexive"
    EqTransitive y z -> object ["transitive" .= object ["y" .= y, "z" .= z]]
    EqNegation y -> object ["negation" .= y]
instance FromJSON a => FromJSON (Operation (AbidesEq a)) where
  parseJSON (Object o) = transitive <|> symmetry <|> negation
    where
      transitive = do
        o' <- o .: "transitive"
        EqTransitive <$> o' .: "y" <*> o' .: "z"
      symmetry = EqSymmetry <$> o .: "symmetry"
      negation = EqNegation <$> o .: "negation"
  parseJSON x@(String s)
    | s == "reflexive" = pure EqReflexive
    | otherwise = typeMismatch "Operation (AbidesEq a)" x
  parseJSON x = typeMismatch "Operation (AbidesEq a)" x
instance Serialize a => Serialize (Operation (AbidesEq a)) where
  put op = case op of
    EqSymmetry y -> putWord8 0 *> put y
    EqReflexive -> putWord8 1
    EqTransitive y z -> putWord8 2 *> put y *> put z
    EqNegation y -> putWord8 3 *> put y
  get = do
    x <- getWord8
    case x of
      0 -> EqSymmetry <$> get
      1 -> pure EqReflexive
      2 -> EqTransitive <$> get <*> get
      3 -> EqNegation <$> get
      _ -> fail "Operation (AbidesEq a)"

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
instance ToJSON a => ToJSON (Operation (AbidesOrd a)) where
  toJSON op = case op of
    OrdReflexive -> String "reflexive"
    OrdAntiSymmetry y -> object ["antisymmetry" .= y]
    OrdTransitive y z -> object ["transitive" .= object ["y" .= y, "z" .= z]]
instance FromJSON a => FromJSON (Operation (AbidesOrd a)) where
  parseJSON (Object o) = transitive <|> antisymmetry
    where
      transitive = do
        o' <- o .: "transitive"
        OrdTransitive <$> o' .: "y" <*> o' .: "z"
      antisymmetry = OrdAntiSymmetry <$> o .: "antisymmetry"
  parseJSON x@(String s)
    | s == "reflexive" = pure OrdReflexive
    | otherwise = typeMismatch "Operation (AbidesOrd a)" x
  parseJSON x = typeMismatch "Operation (AbidesOrd a)" x
instance Serialize a => Serialize (Operation (AbidesOrd a)) where
  put op = case op of
    OrdReflexive -> putWord8 0
    OrdAntiSymmetry y -> putWord8 1 *> put y
    OrdTransitive y z -> putWord8 2 *> put y *> put z
  get = do
    x <- getWord8
    case x of
      0 -> pure OrdReflexive
      1 -> OrdAntiSymmetry <$> get
      2 -> OrdTransitive <$> get <*> get
      _ -> fail "Operation (AbidesOrd a)"

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
instance ToJSON a => ToJSON (Operation (AbidesEnum a)) where
  toJSON op = case op of
    EnumCompareHom y -> object ["compareHom" .= y]
    EnumPredSucc -> String "predsucc"
    EnumSuccPred -> String "succpred"
instance FromJSON a => FromJSON (Operation (AbidesEnum a)) where
  parseJSON (Object o) = EnumCompareHom <$> o .: "compareHom"
  parseJSON x@(String s)
    | s == "predsucc" = pure EnumPredSucc
    | s == "succpred" = pure EnumSuccPred
    | otherwise = typeMismatch "Operation (AbidesEnum a)" x
  parseJSON x = typeMismatch "Operation (AbidesEnum a)" x
instance Serialize a => Serialize (Operation (AbidesEnum a)) where
  put op = case op of
    EnumCompareHom y -> putWord8 0 *> put y
    EnumPredSucc -> putWord8 1
    EnumSuccPred -> putWord8 2
  get = do
    x <- getWord8
    case x of
      0 -> EnumCompareHom <$> get
      1 -> pure EnumPredSucc
      2 -> pure EnumSuccPred
      _ -> fail "Operation (AbidesEnum a)"

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
instance ToJSON a => ToJSON (Operation (AbidesSemiring a)) where
  toJSON op = case op of
    SemiringCommutativeMonoid y z -> object ["commutativeMonoid" .= object ["y" .= y, "z" .= z]]
    SemiringMonoid y z -> object ["monoid" .= object ["y" .= y, "z" .= z]]
    SemiringLeftDistributive y z -> object ["leftDistributive" .= object ["y" .= y, "z" .= z]]
    SemiringRightDistributive y z -> object ["rightDistributive" .= object ["y" .= y, "z" .= z]]
    SemiringAnnihilation -> String "annihilation"
instance FromJSON a => FromJSON (Operation (AbidesSemiring a)) where
  parseJSON (Object o) = commutativeMonoid <|> monoid <|> leftDistributive <|> rightDistributive
    where
      commutativeMonoid = do
        o' <- o .: "commutativeMonoid"
        SemiringCommutativeMonoid <$> o' .: "y" <*> o' .: "z"
      monoid = do
        o' <- o .: "monoid"
        SemiringMonoid <$> o' .: "y" <*> o' .: "z"
      leftDistributive = do
        o' <- o .: "leftDistributive"
        SemiringLeftDistributive <$> o' .: "y" <*> o' .: "z"
      rightDistributive = do
        o' <- o .: "rightDistributive"
        SemiringRightDistributive <$> o' .: "y" <*> o' .: "z"
  parseJSON x@(String s)
    | s == "annihilation" = pure SemiringAnnihilation
    | otherwise = typeMismatch "Operation (AbidesSemiring a)" x
  parseJSON x = typeMismatch "Operation (AbidesSemiring a)" x
instance Serialize a => Serialize (Operation (AbidesSemiring a)) where
  put op = case op of
    SemiringCommutativeMonoid y z -> putWord8 0 *> put y *> put z
    SemiringMonoid y z -> putWord8 1 *> put y *> put z
    SemiringLeftDistributive y z -> putWord8 2 *> put y *> put z
    SemiringRightDistributive y z -> putWord8 3 *> put y *> put z
    SemiringAnnihilation -> putWord8 4
  get = do
    x <- getWord8
    case x of
      0 -> SemiringCommutativeMonoid <$> get <*> get
      1 -> SemiringMonoid <$> get <*> get
      2 -> SemiringLeftDistributive <$> get <*> get
      3 -> SemiringRightDistributive <$> get <*> get
      4 -> pure SemiringAnnihilation
      _ -> fail "Operation (AbidesSemiring a)"

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
instance ToJSON a => ToJSON (Operation (AbidesRing a)) where
  toJSON op = case op of
    RingSemiring op' -> object ["semiring" .= op']
    RingAdditiveInverse -> String "additiveInverse"
instance FromJSON a => FromJSON (Operation (AbidesRing a)) where
  parseJSON (Object o) = RingSemiring <$> o .: "semiring"
  parseJSON x@(String s)
    | s == "additiveInverse" = pure RingAdditiveInverse
    | otherwise = typeMismatch "Operation (AbidesRing a)" x
  parseJSON x = typeMismatch "Operation (AbidesRing a)" x
instance Serialize a => Serialize (Operation (AbidesRing a)) where
  put op = case op of
    RingSemiring op' -> putWord8 0 *> put op'
    RingAdditiveInverse -> putWord8 1
  get = do
    x <- getWord8
    case x of
      0 -> RingSemiring <$> get
      1 -> pure RingAdditiveInverse
      _ -> fail "Operation (AbidesRing a)"

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
instance ToJSON a => ToJSON (Operation (AbidesCommutativeRing a)) where
  toJSON op = case op of
    CommutativeRingRing op' -> object ["ring" .= op']
    CommutativeRingCommutative y -> object ["commutative" .= y]
instance FromJSON a => FromJSON (Operation (AbidesCommutativeRing a)) where
  parseJSON (Object o) = ring <|> commutative
    where
      ring = CommutativeRingRing <$> o .: "ring"
      commutative = CommutativeRingCommutative <$> o .: "commutative"
  parseJSON x = typeMismatch "Operation (AbidesCommutativeRing a)" x
instance Serialize a => Serialize (Operation (AbidesCommutativeRing a)) where
  put op = case op of
    CommutativeRingRing op' -> putWord8 0 *> put op'
    CommutativeRingCommutative y -> putWord8 1 *> put y
  get = do
    x <- getWord8
    case x of
      0 -> CommutativeRingRing <$> get
      1 -> CommutativeRingCommutative <$> get
      _ -> fail "Operation (AbidesCommutativeRing a)"

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
instance ToJSON a => ToJSON (Operation (AbidesDivisionRing a)) where
  toJSON op = case op of
    DivisionRingRing op' -> object ["ring" .= op']
    DivisionRingInverse -> String "inverse"
instance FromJSON a => FromJSON (Operation (AbidesDivisionRing a)) where
  parseJSON (Object o) = DivisionRingRing <$> o .: "ring"
  parseJSON x@(String s)
    | s == "inverse" = pure DivisionRingInverse
    | otherwise = typeMismatch "Operation (AbidesDivisionRing a)" x
  parseJSON x = typeMismatch "Operation (AbidesDivisionRing a)" x
instance Serialize a => Serialize (Operation (AbidesDivisionRing a)) where
  put op = case op of
    DivisionRingRing op' -> putWord8 0 *> put op'
    DivisionRingInverse -> putWord8 1
  get = do
    x <- getWord8
    case x of
      0 -> DivisionRingRing <$> get
      1 -> pure DivisionRingInverse
      _ -> fail "Operation (AbidesDivisionRing a)"

instance (Num a, Eq a) => SymbioteOperation (AbidesEuclideanRing a) Bool where
  data Operation (AbidesEuclideanRing a)
    = EuclideanRingCommutativeRing (Operation (AbidesCommutativeRing a))
    | EuclideanRingIntegralDomain (AbidesEuclideanRing a)
  perform op x@(AbidesEuclideanRing x') = case op of
    EuclideanRingCommutativeRing op' -> perform op' (AbidesCommutativeRing x')
    EuclideanRingIntegralDomain y -> EuclideanRing.integralDomain x y
deriving instance Generic (Operation (AbidesEuclideanRing a))
deriving instance Show a => Show (Operation (AbidesEuclideanRing a))
instance Arbitrary a => Arbitrary (Operation (AbidesEuclideanRing a)) where
  arbitrary = oneof
    [ EuclideanRingCommutativeRing <$> arbitrary
    , EuclideanRingIntegralDomain <$> arbitrary
    ]
instance ToJSON a => ToJSON (Operation (AbidesEuclideanRing a)) where
  toJSON op = case op of
    EuclideanRingCommutativeRing op' -> object ["commutativeRing" .= op']
    EuclideanRingIntegralDomain y -> object ["integralDomain" .= y]
instance FromJSON a => FromJSON (Operation (AbidesEuclideanRing a)) where
  parseJSON (Object o) = commutativeRing <|> integralDomain
    where
      commutativeRing = EuclideanRingCommutativeRing <$> o .: "commutativeRing"
      integralDomain = EuclideanRingIntegralDomain <$> o .: "integralDomain"
  parseJSON x = typeMismatch "Operation (AbidesEuclideanRing a)" x
instance Serialize a => Serialize (Operation (AbidesEuclideanRing a)) where
  put op = case op of
    EuclideanRingCommutativeRing op' -> putWord8 0 *> put op'
    EuclideanRingIntegralDomain y -> putWord8 1 *> put y
  get = do
    x <- getWord8
    case x of
      0 -> EuclideanRingCommutativeRing <$> get
      1 -> EuclideanRingIntegralDomain <$> get
      _ -> fail "Operation (AbidesEuclideanRing a)"

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
instance ToJSON a => ToJSON (Operation (AbidesField a)) where
  toJSON op = case op of
    FieldDivisionRing op' -> object ["divisionRing" .= op']
    FieldEuclideanRing y -> object ["euclideanRing" .= y]
instance FromJSON a => FromJSON (Operation (AbidesField a)) where
  parseJSON (Object o) = divisionRing <|> euclideanRing
    where
      divisionRing = FieldDivisionRing <$> o .: "divisionRing"
      euclideanRing = FieldEuclideanRing <$> o .: "euclideanRing"
  parseJSON x = typeMismatch "Operation (AbidesField a)" x
instance Serialize a => Serialize (Operation (AbidesField a)) where
  put op = case op of
    FieldDivisionRing op' -> putWord8 0 *> put op'
    FieldEuclideanRing y -> putWord8 1 *> put y
  get = do
    x <- getWord8
    case x of
      0 -> FieldDivisionRing <$> get
      1 -> FieldEuclideanRing <$> get
      _ -> fail "Operation (AbidesField a)"
