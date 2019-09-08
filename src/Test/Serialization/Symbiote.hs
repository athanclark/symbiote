{-# LANGUAGE
    MultiParamTypeClasses
  , TypeFamilies
  , ExistentialQuantification
  , RankNTypes
  , ScopedTypeVariables
  , NamedFieldPuns
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , StandaloneDeriving
  , UndecidableInstances
  , FlexibleInstances
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

module Test.Serialization.Symbiote
  ( SymbioteOperation (..), Symbiote (..), EitherOp (..), Topic, SymbioteT, register
  , firstPeer, secondPeer, First (..), Second (..), Generating (..), Operating (..), Failure (..)
  , defaultSuccess, defaultFailure, defaultProgress, nullProgress, simpleTest
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import Data.String (IsString)
import Data.Proxy (Proxy (..))
import Text.Printf (printf)
import Control.Concurrent.STM
  (TVar, newTVarIO, readTVar, readTVarIO, modifyTVar', writeTVar, atomically, newTChan, readTChan, writeTChan)
import Control.Concurrent.Async (async, wait)
import Control.Monad (forever, void)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseWith)
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


-- | The most trivial serialization medium for any @a@.
newtype EitherOp a = EitherOp (Either a (Operation a))
deriving instance (Eq a, Eq (Operation a)) => Eq (EitherOp a)
deriving instance (Show a, Show (Operation a)) => Show (EitherOp a)

instance SymbioteOperation a => Symbiote a (EitherOp a) where
  encode = EitherOp . Left
  decode (EitherOp (Left x)) = Just x
  decode (EitherOp (Right _)) = Nothing
  encodeOp = EitherOp . Right
  decodeOp (EitherOp (Left _)) = Nothing
  decodeOp (EitherOp (Right x)) = Just x

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
        , encode' = encode
        , encodeOp' = encodeOp
        , decode' = decode
        , decodeOp' = decodeOp
        , perform' = perform
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
  | ImFinished -- FIXME once first is finished, it goes into operating, then tries again, but is still finished - will 2nd go into operating after finished?
  | GeneratingNoParseOperated s
  deriving (Eq, Show)

-- | Messages sent by a peer during their operating phase
data Operating s
  = Operated s -- ^ Serialized value after operation
  | OperatingNoParseValue s
  | OperatingNoParseOperation s
  deriving (Eq, Show)

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
  deriving (Eq, Show)

getFirstGenerating :: First s -> Maybe (Topic, Generating s)
getFirstGenerating x = case x of
  FirstGenerating topic g -> Just (topic, g)
  _ -> Nothing

getFirstOperating :: First s -> Maybe (Topic, Operating s)
getFirstOperating x = case x of
  FirstOperating topic g -> Just (topic, g)
  _ -> Nothing


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
  deriving (Eq, Show)

getSecondGenerating :: Second s -> Maybe (Topic, Generating s)
getSecondGenerating x = case x of
  SecondGenerating topic g -> Just (topic, g)
  _ -> Nothing

getSecondOperating :: Second s -> Maybe (Topic, Operating s)
getSecondOperating x = case x of
  SecondOperating topic g -> Just (topic, g)
  _ -> Nothing


data Failure them s
  = BadTopicsFailure
    { badTopicsFirst :: Map Topic Int
    , badTopicsSecond :: Map Topic Int
    }
  | OutOfSyncFirst (First s)
  | OutOfSyncSecond (Second s)
  | TopicNonexistent Topic
  | WrongTopic
    { wrongTopicExpected :: Topic
    , wrongTopicGot :: Topic
    }
  | CantParseOperated Topic s
  | CantParseGeneratedValue Topic s
  | CantParseGeneratedOperation Topic s
  | CantParseLocalValue Topic s
  | CantParseLocalOperation Topic s
  | BadOperating Topic (Operating s)
  | BadGenerating Topic (Generating s)
  | BadThem Topic (them s)
  | SafeFailure
    { safeFailureTopic :: Topic
    , safeFailureExpected :: s
    , safeFailureGot :: s
    }
  deriving (Eq, Show)


-- | Via putStrLn
defaultSuccess :: Topic -> IO ()
defaultSuccess (Topic t) = putStrLn $ "Topic " ++ unpack t ++ " succeeded"

-- | Via putStrLn
defaultFailure :: Show (them s) => Show s => Failure them s -> IO ()
defaultFailure f = error $ "Failure: " ++ show f

-- | Via putStrLn
defaultProgress :: Topic -> Float -> IO ()
defaultProgress (Topic t) p = putStrLn $ "Topic " ++ unpack t ++ ": " ++ (printf "%.2f" (p * 100.0)) ++ "%"

-- | Do nothing
nullProgress :: Topic -> Float -> IO ()
nullProgress _ _ = pure ()


firstPeer :: forall m s
           . MonadIO m
          => Show s
          => (First s -> m ()) -- ^ Encode and send first messages
          -> (m (Second s)) -- ^ Receive and decode second messages
          -> (Topic -> m ()) -- ^ Report when Successful
          -> (Failure Second s -> m ()) -- ^ Report when Failed
          -> (Topic -> Float -> m ()) -- ^ Report on Progress
          -> SymbioteT s m ()
          -> m ()
firstPeer encodeAndSend receiveAndDecode onSuccess onFailure onProgress x = do
  state <- runSymbioteT x True
  let topics = maxSize <$> state
  encodeAndSend (AvailableTopics topics)
  shouldBeStart <- receiveAndDecode
  case shouldBeStart of
    BadTopics badTopics -> onFailure $ BadTopicsFailure topics badTopics
    Start -> do
      topicsToProcess <- liftIO (newTVarIO (Map.keysSet topics))
      let processAllTopics = do
            mTopicToProcess <- Set.maxView <$> liftIO (readTVarIO topicsToProcess)
            case mTopicToProcess of
              Nothing -> pure () -- done
              Just (topic, newTopics) -> do
                liftIO (atomically (writeTVar topicsToProcess newTopics))
                case Map.lookup topic state of
                  Nothing -> onFailure $ TopicNonexistent topic
                  Just symbioteState -> do
                    hasSentFinishedVar <- liftIO $ newTVarIO HasntSentFinished
                    hasReceivedFinishedVar <- liftIO $ newTVarIO HasntReceivedFinished
                    generating
                      encodeAndSend receiveAndDecode
                      FirstGenerating FirstOperating
                      getSecondGenerating getSecondOperating
                      hasSentFinishedVar hasReceivedFinishedVar
                      processAllTopics
                      onSuccess
                      onFailure
                      onProgress
                      topic symbioteState
      processAllTopics
    _ -> onFailure $ OutOfSyncSecond shouldBeStart


secondPeer :: forall s m
            . MonadIO m
           => Show s
           => (Second s -> m ()) -- ^ Encode and send second messages
           -> (m (First s)) -- ^ Receive and decode first messages
           -> (Topic -> m ()) -- ^ Report when Successful
           -> (Failure First s -> m ()) -- ^ Report when Failed
           -> (Topic -> Float -> m ()) -- ^ Report on Progress
           -> SymbioteT s m ()
           -> m ()
secondPeer encodeAndSend receiveAndDecode onSuccess onFailure onProgress x = do
  state <- runSymbioteT x False
  shouldBeAvailableTopics <- receiveAndDecode
  case shouldBeAvailableTopics of
    AvailableTopics topics -> do
      let myTopics = maxSize <$> state
      if myTopics /= topics
        then do
          encodeAndSend (BadTopics myTopics)
          onFailure $ BadTopicsFailure topics myTopics
        else do
          encodeAndSend Start
          topicsToProcess <- liftIO (newTVarIO (Map.keysSet topics))
          let processAllTopics = do
                mTopicToProcess <- Set.maxView <$> liftIO (readTVarIO topicsToProcess)
                case mTopicToProcess of
                  Nothing -> pure () -- done
                  Just (topic, newTopics) -> do
                    liftIO (atomically (writeTVar topicsToProcess newTopics))
                    case Map.lookup topic state of
                      Nothing -> onFailure $ TopicNonexistent topic
                      Just symbioteState -> do
                        hasSentFinishedVar <- liftIO $ newTVarIO HasntSentFinished
                        hasReceivedFinishedVar <- liftIO $ newTVarIO HasntReceivedFinished
                        operating
                          encodeAndSend receiveAndDecode
                          SecondGenerating SecondOperating
                          getFirstGenerating getFirstOperating
                          hasSentFinishedVar hasReceivedFinishedVar
                          processAllTopics
                          onSuccess
                          onFailure
                          onProgress
                          topic symbioteState
          processAllTopics
    _ -> onFailure $ OutOfSyncFirst shouldBeAvailableTopics


data HasSentFinished
  = HasSentFinished
  | HasntSentFinished

data HasReceivedFinished
  = HasReceivedFinished
  | HasntReceivedFinished


generating :: MonadIO m
           => Show s
           => (me s -> m ()) -- ^ Encode and send first messages
           -> (m (them s)) -- ^ Receive and decode second messages
           -> (Topic -> Generating s -> me s) -- ^ Build a generating datum, whether first or second
           -> (Topic -> Operating s -> me s) -- ^ Build a generating datum, whether first or second
           -> (them s -> Maybe (Topic, Generating s)) -- ^ Deconstruct an operating datum, whether first or second
           -> (them s -> Maybe (Topic, Operating s)) -- ^ Deconstruct an operating datum, whether first or second
           -> TVar HasSentFinished
           -> TVar HasReceivedFinished
           -> m () -- ^ on finished - loop
           -> (Topic -> m ()) -- ^ report topic success
           -> (Failure them s -> m ()) -- ^ report topic failure
           -> (Topic -> Float -> m ()) -- ^ report topic progress
           -> Topic
           -> SymbioteState s
           -> m ()
generating
  encodeAndSend receiveAndDecode
  makeGen makeOp
  getGen getOp
  hasSentFinishedVar hasReceivedFinishedVar
  onFinished
  onSuccess
  onFailure
  onProgress
  topic symbioteState@SymbioteState{equal,encode'} = do
  mGenerated <- generateSymbiote symbioteState
  case mGenerated of
    DoneGenerating -> do
      encodeAndSend $ makeGen topic ImFinished
      liftIO $ atomically $ writeTVar hasSentFinishedVar HasSentFinished
      operatingTryFinished
    GeneratedSymbiote
      { generatedValue = generatedValueEncoded
      , generatedOperation = generatedOperationEncoded
      } -> do
      -- send
      encodeAndSend $ makeGen topic $ Generated
        { genValue = generatedValueEncoded
        , genOperation = generatedOperationEncoded
        }
      -- receive
      shouldBeOperating <- receiveAndDecode
      case getOp shouldBeOperating of
        Just (secondOperatingTopic, shouldBeOperated)
          | secondOperatingTopic /= topic ->
            onFailure $ WrongTopic topic secondOperatingTopic
          | otherwise -> case shouldBeOperated of
              Operated operatedValueEncoded -> case decode operatedValueEncoded of
                Nothing -> do
                  encodeAndSend $ makeGen topic $ GeneratingNoParseOperated operatedValueEncoded
                  onFailure $ CantParseOperated topic operatedValueEncoded
                Just operatedValue -> case decode generatedValueEncoded of
                  Nothing -> onFailure $ CantParseLocalValue topic generatedValueEncoded
                  Just generatedValue -> case decodeOp generatedOperationEncoded of
                    Nothing -> onFailure $ CantParseLocalOperation topic generatedOperationEncoded
                    Just generatedOperation -> do
                      -- decoded operated value, generated value & operation
                      let expected = perform generatedOperation generatedValue
                      if equal expected operatedValue
                        then do
                          encodeAndSend $ makeGen topic YourTurn
                          progress <- getProgress symbioteState
                          (onProgress topic progress)
                          operating
                            encodeAndSend receiveAndDecode
                            makeGen makeOp
                            getGen getOp
                            hasSentFinishedVar hasReceivedFinishedVar
                            onFinished
                            onSuccess
                            onFailure
                            onProgress
                            topic symbioteState
                        else do
                          encodeAndSend $ makeGen topic $ BadResult operatedValueEncoded
                          onFailure $ SafeFailure topic (encode' expected) operatedValueEncoded
              _ -> onFailure $ BadOperating topic shouldBeOperated
        _ -> onFailure $ BadThem topic shouldBeOperating
  where
    operatingTryFinished = do
      hasReceivedFinished <- liftIO $ readTVarIO hasReceivedFinishedVar
      case hasReceivedFinished of
        HasReceivedFinished -> do
          (onSuccess topic)
          onFinished -- stop cycling - last generation in sequence is from second
        HasntReceivedFinished -> do
          progress <- getProgress symbioteState
          (onProgress topic progress)
          operating
            encodeAndSend receiveAndDecode
            makeGen makeOp
            getGen getOp
            hasSentFinishedVar hasReceivedFinishedVar
            onFinished
            onSuccess
            onFailure
            onProgress
            topic symbioteState

operating :: MonadIO m
          => Show s
          => (me s -> m ()) -- ^ Encode and send first messages
          -> (m (them s)) -- ^ Receive and decode second messages
          -> (Topic -> Generating s -> me s) -- ^ Build a generating datum, whether first or second
          -> (Topic -> Operating s -> me s) -- ^ Build a generating datum, whether first or second
          -> (them s -> Maybe (Topic, Generating s)) -- ^ Deconstruct an operating datum, whether first or second
          -> (them s -> Maybe (Topic, Operating s)) -- ^ Deconstruct an operating datum, whether first or second
          -> TVar HasSentFinished
          -> TVar HasReceivedFinished
          -> m () -- ^ on finished
          -> (Topic -> m ()) -- ^ report topic success
          -> (Failure them s -> m ()) -- ^ report topic failure
          -> (Topic -> Float -> m ()) -- ^ report topic progress
          -> Topic
          -> SymbioteState s
          -> m ()
operating
  encodeAndSend receiveAndDecode
  makeGen makeOp
  getGen getOp
  hasSentFinishedVar hasReceivedFinishedVar
  onFinished
  onSuccess
  onFailure
  onProgress
  topic symbioteState@SymbioteState{decode',decodeOp',perform',encode'} = do
  shouldBeGenerating <- receiveAndDecode
  case getGen shouldBeGenerating of
    Just (secondGeneratingTopic,shouldBeGenerated)
      | secondGeneratingTopic /= topic ->
        onFailure $ WrongTopic topic secondGeneratingTopic
      | otherwise -> case shouldBeGenerated of
          ImFinished -> do
            liftIO $ atomically $ writeTVar hasReceivedFinishedVar HasReceivedFinished
            generatingTryFinished
          YourTurn -> do
            progress <- getProgress symbioteState
            (onProgress topic progress)
            generating
              encodeAndSend receiveAndDecode
              makeGen makeOp
              getGen getOp
              hasSentFinishedVar hasReceivedFinishedVar
              onFinished
              onSuccess
              onFailure
              onProgress
              topic symbioteState
          Generated
            { genValue = generatedValueEncoded
            , genOperation = generatedOperationEncoded
            } -> case decode' generatedValueEncoded of
            Nothing -> do
              encodeAndSend $ makeOp topic $ OperatingNoParseValue generatedValueEncoded
              onFailure $ CantParseGeneratedValue topic generatedValueEncoded
            Just generatedValue -> case decodeOp' generatedOperationEncoded of
              Nothing -> do
                encodeAndSend $ makeOp topic $ OperatingNoParseValue generatedOperationEncoded
                onFailure $ CantParseGeneratedOperation topic generatedOperationEncoded
              Just generatedOperation -> do
                encodeAndSend $ makeOp topic $ Operated $ encode' $ perform' generatedOperation generatedValue
                -- wait for response
                operating
                  encodeAndSend
                  receiveAndDecode
                  makeGen makeOp
                  getGen getOp
                  hasSentFinishedVar hasReceivedFinishedVar
                  onFinished
                  onSuccess
                  onFailure
                  onProgress
                  topic symbioteState
          _ -> onFailure $ BadGenerating topic shouldBeGenerated
    _ -> onFailure $ BadThem topic shouldBeGenerating
  where
    generatingTryFinished = do
      hasSentFinished <- liftIO $ readTVarIO hasSentFinishedVar
      case hasSentFinished of
        HasSentFinished -> do
          (onSuccess topic)
          onFinished -- stop cycling - last operation in sequence is from first
        HasntSentFinished -> do
          progress <- getProgress symbioteState
          (onProgress topic progress)
          generating
            encodeAndSend receiveAndDecode
            makeGen makeOp
            getGen getOp
            hasSentFinishedVar hasReceivedFinishedVar
            onFinished
            onSuccess
            onFailure
            onProgress
            topic symbioteState


-- | Prints to stdout and uses a local channel for a sanity-check - doesn't serialize.
simpleTest :: MonadBaseControl IO m
           => MonadIO m
           => Show a
           => Show (Operation a)
           => SymbioteT (EitherOp a) m () -> m ()
simpleTest suite = do
  firstChan <- liftIO $ atomically newTChan
  secondChan <- liftIO $ atomically newTChan

  t <- liftBaseWith $ \runInBase -> async $
    void $ runInBase $ firstPeer
      (encodeAndSendChan firstChan)
      (receiveAndDecodeChan secondChan)
      (const (pure ())) (liftIO . defaultFailure) (\a b -> liftIO $ nullProgress a b)
      suite
  secondPeer
    (encodeAndSendChan secondChan)
    (receiveAndDecodeChan firstChan)
    (const (pure ())) (liftIO . defaultFailure) (\a b -> liftIO $ nullProgress a b)
    suite
  liftIO (wait t)
  where
    encodeAndSendChan chan x = liftIO $ atomically (writeTChan chan x)
    receiveAndDecodeChan chan = liftIO $ atomically (readTChan chan)
