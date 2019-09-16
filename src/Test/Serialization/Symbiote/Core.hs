{-# LANGUAGE
    ExistentialQuantification
  , NamedFieldPuns
  , RankNTypes
  , ScopedTypeVariables
  , TypeFamilies
  , MultiParamTypeClasses
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  #-}

module Test.Serialization.Symbiote.Core where

import Data.Text (Text)
import Data.String (IsString)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.STM
  (TVar, readTVar, readTVarIO, modifyTVar', atomically)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, execStateT)
import Test.QuickCheck.Arbitrary (Arbitrary)
import Test.QuickCheck.Gen (Gen, resize)
import qualified Test.QuickCheck.Gen as QC


-- | A type-level relation between a type and appropriate, testable operations on that type.
class SymbioteOperation a where
  data Operation a :: *
  perform :: Operation a -> a -> a

-- | A serialization format for a particular type, and serialized data type.
class SymbioteOperation a => Symbiote a s where
  encode   :: a -> s
  decode   :: s -> Maybe a
  encodeOp :: Operation a -> s
  decodeOp :: s -> Maybe (Operation a)


-- | Unique name of a type, for a suite of tests
newtype Topic = Topic Text
  deriving (Eq, Ord, Show, IsString)

-- | Protocol state for a particular topic
data SymbioteProtocol a s
  = MeGenerated
    { meGenValue :: a
    , meGenOperation :: Operation a
    , meGenReceived :: Maybe s
    }
  | ThemGenerating
    { themGen :: Maybe (s, s)
    }
  | NotStarted
  | Finished

-- | Protocol generation state
data SymbioteGeneration a s = SymbioteGeneration
  { size     :: Int
  , protocol :: SymbioteProtocol a s
  }

newGeneration :: SymbioteGeneration a s
newGeneration = SymbioteGeneration
  { size = 1
  , protocol = NotStarted
  }


-- | Internal existential state of a registered topic with type's facilities
data SymbioteState s =
  forall a
  . ( Arbitrary a
    , Arbitrary (Operation a)
    , Symbiote a s
    , Eq a
    ) =>
  SymbioteState
  { generate   :: Gen a
  , generateOp :: Gen (Operation a)
  , equal      :: a -> a -> Bool
  , maxSize    :: Int
  , generation :: TVar (SymbioteGeneration a s)
  , encode'    :: a -> s
  , encodeOp'  :: Operation a -> s
  , decode'    :: s -> Maybe a
  , decodeOp'  :: s -> Maybe (Operation a)
  , perform'   :: Operation a -> a -> a
  }


type SymbioteT s m = ReaderT Bool (StateT (Map Topic (SymbioteState s)) m)

runSymbioteT :: Monad m
             => SymbioteT s m ()
             -> Bool -- ^ Is this the first peer to initiate the protocol?
             -> m (Map Topic (SymbioteState s))
runSymbioteT x isFirst = execStateT (runReaderT x isFirst) Map.empty


data GenerateSymbiote s
  = DoneGenerating
  | GeneratedSymbiote
    { generatedValue :: s
    , generatedOperation :: s
    }


generateSymbiote :: forall s m. MonadIO m => SymbioteState s -> m (GenerateSymbiote s)
generateSymbiote SymbioteState{generate,generateOp,maxSize,generation} = do
  let go g@SymbioteGeneration{size} = g {size = size + 1}
  SymbioteGeneration{size} <- liftIO $ atomically $ modifyTVar' generation go *> readTVar generation
  if size >= maxSize
    then pure DoneGenerating
    else do
      let genResize :: forall q. Gen q -> m q
          genResize = liftIO . QC.generate . resize size
      generatedValue <- encode <$> genResize generate
      generatedOperation <- encodeOp <$> genResize generateOp
      pure GeneratedSymbiote{generatedValue,generatedOperation}


getProgress :: MonadIO m => SymbioteState s -> m Float
getProgress SymbioteState{maxSize,generation} = do
  SymbioteGeneration{size} <- liftIO $ readTVarIO generation
  pure $ fromIntegral size / fromIntegral maxSize
