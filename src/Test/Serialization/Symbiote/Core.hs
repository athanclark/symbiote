{-# LANGUAGE
    RankNTypes
  , TypeFamilies
  , NamedFieldPuns
  , FlexibleContexts
  , ScopedTypeVariables
  , MultiParamTypeClasses
  , FunctionalDependencies
  , ExistentialQuantification
  , GeneralizedNewtypeDeriving
  #-}

{-|

Module: Test.Serialization.Symbiote.Core
Copyright: (c) 2019 Athan Clark
License: BSD-3-Style
Maintainer: athan.clark@gmail.com
Portability: GHC

This module defines the machinery of any topic's state machine:

* agnostic for whatever type or operation you're testing
* doesn't try to define the stuff that orchestrates multiple topics operating together
* doesn't specify how the protocol actually looks with respect to the messages sent between the peers

-}

module Test.Serialization.Symbiote.Core where

import Data.Text (Text, unpack, pack, length)
import Data.String (IsString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Int (Int32)
import Data.Proxy (Proxy (..))
import Data.Traversable (traverse)
import Data.Aeson (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
import Data.Serialize (Serialize (..))
import Data.Serialize.Put (putInt32be)
import Data.Serialize.Get (getInt32be)
import Control.Monad (void, replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.STM
  (TVar, readTVar, readTVarIO, modifyTVar', atomically)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, execStateT)
import Test.QuickCheck.Arbitrary (Arbitrary)
import Test.QuickCheck.Gen (Gen, resize)
import Test.QuickCheck.Instances ()
import qualified Test.QuickCheck.Gen as QC


-- | A type-level relation between a type and appropriate, testable operations on that type.
class SymbioteOperation a o | a -> o where
  -- | An enumerated type of operations on @a@ that result in @o@.
  data Operation a :: *
  -- | Apply the 'Operation' to @a@, to get an @o@.
  perform :: Operation a -> a -> o

-- | A serialization format for a particular type, and serialized data type.
class SymbioteOperation a o => Symbiote a o s | a -> o where
  encode    :: a -> s
  decode    :: s -> Maybe a
  -- | Needs a reference to @a@ because the fundep is only one direction (i.e. only one output defined per input, but could be used elsewhere)
  encodeOut :: Proxy a -> o -> s
  -- | Needs a reference to @a@ because the fundep is only one direction
  decodeOut :: Proxy a -> s -> Maybe o
  encodeOp  :: Operation a -> s
  decodeOp  :: s -> Maybe (Operation a)


-- | Unique name of a type, for a suite of tests
newtype Topic = Topic Text
  deriving (Eq, Ord, Show, IsString, Arbitrary, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
-- | Serialized as a @String32@ in the <https://symbiotic-data.github.io/#/data/?id=string32 symbiotic-data standard>.
instance Serialize Topic where
  put (Topic t) = do
    putInt32be (fromIntegral (Data.Text.length t))
    void (traverse put (unpack t))
  get = do
    l <- getInt32be
    Topic . pack <$> replicateM (fromIntegral l) get

-- | The state-machine representation for a particular topic, identical for each peer. As the protocol progresses and messages are passed, and values are generated, this
-- value will change over time, and is unique for each topic.
data SymbioteProtocol a s
  = -- | \"I\'m generating a value\"
    MeGenerated
    { meGenValue     :: a -- ^ The value generated
    , meGenOperation :: Operation a -- ^ The operation generated
    , meGenReceived  :: Maybe s -- ^ Remotely operated value - might not exist due to the need for the remote party to send the @o@ output value.
    }
  | -- | \"They\'re generating a value\"
    ThemGenerating
    { themGen :: Maybe (s, s) -- ^ Remotely generated value and operation - might not exist due to the need for the remote party to send the @a@ and 'Operation' values.
    }
  | -- | The topic's generation / operation exchange hasn't started for either peer
    NotStarted
    -- | The topic's generation / operation exchange has completed, and is currently being processed on another topic, or the whole suite is finished for all topics.
  | Finished

-- | Protocol state, with amended \"current generation value\" information
data SymbioteGeneration a s = SymbioteGeneration
  { size     :: Int32 -- ^ The current \"size\" for the generated values and operations, with respect to QuickCheck\'s <http://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck-Gen.html#v:sized size> parameter.
  , protocol :: SymbioteProtocol a s -- ^ The rest of the state
  }

-- | The initial state for any topic\'s state machine.
newGeneration :: SymbioteGeneration a s
newGeneration = SymbioteGeneration
  { size = 1
  , protocol = NotStarted
  }


-- | Internal existential state of a registered topic, with fields explicitly storing a type's normally typeclass-specified facilities.
data SymbioteState a o s =
  SymbioteState
  { generate   :: Gen a -- ^ Generate a random value
  , generateOp :: Gen (Operation a) -- ^ Generate a random operation
  , equal      :: o -> o -> Bool -- ^ Are the outputs equal?
  , maxSize    :: Int32 -- ^ Maximum size the internal generation will approach to - with respect to QuickCheck\'s <http://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck-Gen.html#v:sized size> parameter
  , generation :: TVar (SymbioteGeneration a s) -- ^ The actual state of the topic, stored as an atomically modifiable 'TVar'
  , encode'    :: a -> s -- ^ 'encode'
  , encodeOut' :: o -> s -- ^ 'encodeOut'
  , encodeOp'  :: Operation a -> s -- ^ 'encodeOp'
  , decode'    :: s -> Maybe a -- ^ 'decode'
  , decodeOut' :: s -> Maybe o -- ^ 'decodeOut'
  , decodeOp'  :: s -> Maybe (Operation a) -- ^ 'decodeOp'
  , perform'   :: Operation a -> a -> o -- ^ 'perform'
  }


-- | A custom existential quantifier, which places additional typeclass constraints on the @a@ and @o@ variables of 'SymbioteState', leaving only @s@ as the globally visible type variable - the \"serialization\" medium.
data ExistsSymbiote s =
  forall a o
  . ( Arbitrary a
    , Arbitrary (Operation a)
    , Symbiote a o s
    , Eq o
    ) =>
  ExistsSymbiote (SymbioteState a o s)

-- | Builder for the total set of topics supported by this peer.
type SymbioteT s m = ReaderT Bool (StateT (Map Topic (ExistsSymbiote s)) m)

-- | Get the set of topics from the builder.
runSymbioteT :: Monad m
             => SymbioteT s m ()
             -> Bool -- ^ Is this the first peer to initiate the protocol?
             -> m (Map Topic (ExistsSymbiote s))
runSymbioteT x isFirst = execStateT (runReaderT x isFirst) Map.empty


-- | Used internally - attempt generating a value and operation for a specific topic, by using the mechanics defined in the protocol's stored state and existential typeclass facilities.
data GenerateSymbiote s
  = -- | Can\'t generate any more values, and \"is /finished/\"
    DoneGenerating
  | -- | Generated a value and operation, as serialized values
    GeneratedSymbiote
    { generatedValue :: s
    , generatedOperation :: s
    }


-- | Given an existentially stored topic state, attempt to generate a value and operation for that topic's type.
generateSymbiote :: forall s m. MonadIO m => ExistsSymbiote s -> m (GenerateSymbiote s)
generateSymbiote (ExistsSymbiote SymbioteState{generate,generateOp,maxSize,generation}) = do
  let go g@SymbioteGeneration{size} = g {size = size + 1}
  SymbioteGeneration{size} <- liftIO $ atomically $ modifyTVar' generation go *> readTVar generation
  if size >= maxSize
    then pure DoneGenerating
    else do
      let genResize :: forall q. Gen q -> m q
          genResize = liftIO . QC.generate . resize (fromIntegral size)
      generatedValue <- encode <$> genResize generate
      generatedOperation <- encodeOp <$> genResize generateOp
      pure GeneratedSymbiote{generatedValue,generatedOperation}

-- | The current \"progress\" or \"percent complete\" of the topic - the current 'size' divided by the 'maxSize'.
getProgress :: MonadIO m => ExistsSymbiote s -> m Float
getProgress (ExistsSymbiote SymbioteState{maxSize,generation}) = do
  SymbioteGeneration{size} <- liftIO (readTVarIO generation)
  pure (fromIntegral size / fromIntegral maxSize)
