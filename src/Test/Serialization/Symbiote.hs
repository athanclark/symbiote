{-# LANGUAGE
    MultiParamTypeClasses
  , TypeFamilies
  #-}

{-|

The project operates as follows:

Given two peers A and B and some communications transport T (utilizing a serialization format S),
and a data type Q with some set of operations on that data type Op_Q,
the following functions / procedures are assumed:

tAB: the function communicates some data in S from peer A to peer B
tBA: the function communicates some data in S from peer B to peer A
encode :: Q -> S
decode :: S -> Q -- disregarding error handling

And the following property should exist, from peer A's perspective:

forall f in Op_Q, q in Q.
  f q == decode (tBA (f (tAB (encode q)))

where the left invocation of f occurs in peer A, and the right invocation occurs in peer B.

-}

module Test.Serialization.Symbiote where

import Data.Map (Map)
import Data.Text (Text)

-- | A type and operation set over a serialization format
class Symbiote a s where
  type Operation a :: * -> *
  perform  :: Operation a -> a -> a
  encode   :: a -> s
  decode   :: s -> Maybe a
  encodeOp :: Operation a -> s
  decodeOp :: s -> Maybe (Operation a)

-- | Unique name of a type, for a suite of tests
newtype Topic = Topic Text
  deriving (Eq, Ord, Show, IsString)

-- | Protocol state for a particular topic
data SymbioteProtocol a s
  = MeGenerating
    { meGenValue :: a
    , meGenOperation :: Operation a
    , meGenReceived :: s
    }
  | ThemGenerating
    { themGen :: Maybe (s, s)
    }

-- | Protocol generation state
type SymbioteGeneration a s = SymbioteGeneration
  { size     :: Int
  , protocol :: SymbioteProtocol a s
  }

-- | Internal existential state of a registered topic with type's facilities
data SymbioteState s =
  forall a
  . ( Arbitrary a
    , Arbitrary (Operation a)
    , Symbiote a s
    , Eq a
    )
  SymbioteState
  { generate   :: Gen a
  , generateOp :: Gen (Operation a)
  , eq         :: a -> a -> Bool
  , generation :: TVar (SymbioteGeneration a s)
  }

-- | Register a topic in the test suite
register :: Arbitrary a
         => Arbitrary (Operation a)
         -> Topic
         -> StateT (Map Topic (SymbioteState s)) m ()

-- | Run the test suite over the transport
execute :: (s -> IO ()) -- ^ transport out
        -> (IO s) -- ^ transport in
        -> StateT (Map Topic (SymbioteState s)) m ()
        -> m ()
