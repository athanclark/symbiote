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
  data Operation a :: *
  perform :: Operation a -> a -> o

-- | A serialization format for a particular type, and serialized data type.
class SymbioteOperation a o => Symbiote a o s | a -> o where
  encode    :: a -> s
  decode    :: s -> Maybe a
  encodeOut :: Proxy a -> o -> s
  decodeOut :: Proxy a -> s -> Maybe o
  encodeOp  :: Operation a -> s
  decodeOp  :: s -> Maybe (Operation a)


-- | Unique name of a type, for a suite of tests
newtype Topic = Topic Text
  deriving (Eq, Ord, Show, IsString, Arbitrary, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
instance Serialize Topic where
  put (Topic t) = do
    putInt32be (fromIntegral (Data.Text.length t))
    void (traverse put (unpack t))
  get = do
    l <- getInt32be
    Topic . pack <$> replicateM (fromIntegral l) get

-- | Protocol state for a particular topic
data SymbioteProtocol a s
  = MeGenerated
    { meGenValue     :: a
    , meGenOperation :: Operation a
    , meGenReceived  :: Maybe s -- ^ Remotely operated value
    }
  | ThemGenerating
    { themGen :: Maybe (s, s) -- ^ Remotely generated value and operation
    }
  | NotStarted
  | Finished

-- | Protocol generation state
data SymbioteGeneration a s = SymbioteGeneration
  { size     :: Int32
  , protocol :: SymbioteProtocol a s
  }

newGeneration :: SymbioteGeneration a s
newGeneration = SymbioteGeneration
  { size = 1
  , protocol = NotStarted
  }


-- | Internal existential state of a registered topic with type's facilities
data SymbioteState a o s =
  SymbioteState
  { generate   :: Gen a
  , generateOp :: Gen (Operation a)
  , equal      :: o -> o -> Bool
  , maxSize    :: Int32
  , generation :: TVar (SymbioteGeneration a s)
  , encode'    :: a -> s
  , encodeOut' :: o -> s
  , encodeOp'  :: Operation a -> s
  , decode'    :: s -> Maybe a
  , decodeOut' :: s -> Maybe o
  , decodeOp'  :: s -> Maybe (Operation a)
  , perform'   :: Operation a -> a -> o
  }


data ExistsSymbiote s =
  forall a o
  . ( Arbitrary a
    , Arbitrary (Operation a)
    , Symbiote a o s
    , Eq o
    ) =>
  ExistsSymbiote (SymbioteState a o s)


type SymbioteT s m = ReaderT Bool (StateT (Map Topic (ExistsSymbiote s)) m)

runSymbioteT :: Monad m
             => SymbioteT s m ()
             -> Bool -- ^ Is this the first peer to initiate the protocol?
             -> m (Map Topic (ExistsSymbiote s))
runSymbioteT x isFirst = execStateT (runReaderT x isFirst) Map.empty


data GenerateSymbiote s
  = DoneGenerating
  | GeneratedSymbiote
    { generatedValue :: s
    , generatedOperation :: s
    }


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


getProgress :: MonadIO m => ExistsSymbiote s -> m Float
getProgress (ExistsSymbiote SymbioteState{maxSize,generation}) = do
  SymbioteGeneration{size} <- liftIO $ readTVarIO generation
  pure $ fromIntegral size / fromIntegral maxSize
