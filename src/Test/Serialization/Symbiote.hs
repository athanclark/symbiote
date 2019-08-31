{-# LANGUAGE
    MultiParamTypeClasses
  , TypeFamilies
  , ExistentialQuantification
  , RankNTypes
  , ScopedTypeVariables
  , NamedFieldPuns
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
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
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.String (IsString)
import Data.Proxy (Proxy (..))
import Control.Concurrent.STM (TVar, newTVarIO, readTVar, readTVarIO, modifyTVar', atomically)
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, modify', execStateT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, resize)
import qualified Test.QuickCheck.Gen as QC


class SymbioteOperation a where
  data Operation a :: *
  perform :: Operation a -> a -> a

-- | A type and operation set over a serialization format
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


generateSymbiote :: forall s m. MonadIO m => SymbioteState s -> Topic -> m (GenerateSymbiote s)
generateSymbiote SymbioteState{generate,generateOp,maxSize,generation} topic = do
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


-- | Register a topic in the test suite
register :: forall a s m
          . Arbitrary a
         => Arbitrary (Operation a)
         => Symbiote a s
         => Eq a
         => MonadIO m
         => Topic
         -> Int -- ^ Max size
         -> Proxy a
         -> SymbioteT s m ()
register t maxSize Proxy = do
  generation <- liftIO (newTVarIO newGeneration)
  let newState :: SymbioteState s
      newState = SymbioteState
        { generate = arbitrary :: Gen a
        , generateOp = arbitrary :: Gen (Operation a)
        , equal = (==) :: a -> a -> Bool
        , maxSize
        , generation
        }
  modify' (Map.insert t newState)

-- | Messages sent by a peer during their generating phase
data Generating s
  = Generated
    { genValue :: s
    , genOperation :: s
    }
  | BadResult s -- ^ Expected value
  | YourTurn
  | GeneratingNoParseOperated s

-- | Messages sent by a peer during their operating phase
data Operating s
  = Operated s -- ^ Serialized value after operation
  | OperatingNoParseValue s
  | OperatingNoParseOperation s

-- | Messages sent by the first peer
data First s
  = AvailableTopics (Map Topic Int) -- ^ Mapping of topics to their gen size
  | FirstGenerating
    { firstGeneratingTopic :: Topic
    , firstGenerating :: Generating s
    }
  | FirstOperating
    { firstOperatingTopic :: Topic
    , firstOperating :: Operating s
    }

-- | Messages sent by the second peer
data Second s
  = BadTopics (Map Topic Int) -- ^ Second's available topics with identical gen sizes
  | Start
  | SecondOperating
    { secondOperatingTopic :: Topic
    , secondOperating :: Operating s
    }
  | SecondGenerating
    { secondGeneratingTopic :: Topic
    , secondGenerating :: Generating s
    }


firstPeer :: (First s -> m ()) -- ^ Encode and send first messages
          -> (m (Second s)) -- ^ Receive and decode second messages
          -> SymbioteT s m ()
          -> m ()
firstPeer encodeAndSend receiveAndDecode x = do
  state <- runSymbioteT x True
  let topics = maxSize <$> state
  encodeAndSend (AvailableTopics topics)
  shouldBeStart <- receiveAndDecode
  case shouldBeStart of
    BadTopics badTopics ->
      error $ "Bad topics - available topics: " ++ show topics ++ ", bad topics: " ++ show badTopics
    Start -> do
      topicsToProcess <- liftIO (newTVarIO topics)
      let loop = do
            mTopicToProcess <- Set.maxView <$> liftIO (atomically (readTVar topics))
            case mTopicToProcess of
              Nothing -> pure () -- done?
              Just (topic, newTopics) -> do
                liftIO (atomically (writeTVar newTopics))
                case Map.lookup topic state of
                  Nothing -> error $ "Broken internally, topic " ++ show topic ++ " does not exist."
                  Just symbioteState -> generating topic symbioteState
      loop
    _ -> error $ "Broken internally. Message received: " ++ show shouldBeStart
  where
    generating :: Topic -> SymbioteState s -> m ()
    generating topic symbioteState = do
      mGenerated <- generateSymbiote symbioteState topic
      case mGenerated of
        DoneGenerating -> undefined -- wait for second?
        GeneratedSymbiote
          { generatedValue = generatedValueEncoded
          , generatedOperation = generatedOperationEncoded
          } -> do
          encodeAndSend $ FirstGenerating
            { firstGeneratingTopic = topic
            , firstGenerating = Generated
              { genValue = generatedValueEncoded
              , genOperation = generatedOperationEncoded
              }
            }
          shouldBeOperating <- receiveAndDecode
          case shouldBeOperating of
            SecondOperating {secondOperatingTopic,secondOperating = shouldBeOperated}
              | secondOperatingTopic /= topic -> error $ "Broken internally. Wrong topic - expected: " ++ show topic ++ ", received: " ++ show secondOperatingTopic
              | otherwise -> case shouldBeOperated of
                  Operated operatedValueEncoded -> case decode operatedValueEncoded of
                    Nothing -> do
                      encodeAndSend $ FirstGenerating topic $ GeneratingNoParseOperated operatedValueEncoded
                      error $ "Couldn't parse: " ++ show s
                    Just operatedValue -> case decode generatedValueEncoded of
                      Nothing -> error $ "Broken internally - couldn't decode encoded local value: " ++ generatedValueEncoded
                      Just generatedValue -> case decodeOp generatedOperationEncoded of
                        Nothing -> error $ "Broken internally - couldn't decode encoded local operation: " ++ generatedOperationEncoded
                        Just generatedOperation ->
                          encodeAndSend $ FirstGenerating topic $
                            if equal (perform generatedOperation generatedValue) operatedValue
                              then YourTurn
                              else BadResult operatedValueEncoded
                  _ -> error $ "Broken internally - something couldn't parse. Received: " ++ show shouldBeOperated
            _ -> error $ "Broken internally. Expected operating, received: " ++ show shouldBeOperating


secondPeer :: (Second s -> m ()) -- ^ Encode and send second messages
           -> (m (First s)) -- ^ Receive and decode first messages
           -> SymbioteT s m ()
           -> m ()
secondPeer encodeAndSend receiveAndDecode x = do
  state <- runSymbioteT x False
  shouldBeAvailableTopics <- receiveAndDecode
  case shouldBeAvailableTopics of
    AvailableTopics topics -> do -- good
      let myTopics = maxSize <$> state
      if myTopics /= topics
        then do -- fail
          encodeAndSend (BadTopics myTopics)
          -- kill self :(
        else do
          encodeAndSend Start
          let loop = forever $ do
                next <- receiveAndDecode
                case next of
                  AvailableTopics _ ->
                    error $ "Broken internally. Message received: " ++ show next
                  FirstGenerating {firstGeneratingTopic,firstGenerating} -> undefined
                  FirstOperating {firstOperatingTopic,firstOperating} -> undefined
          loop
    _ -> undefined -- something's broken internally
