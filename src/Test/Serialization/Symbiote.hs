{-# LANGUAGE
    RankNTypes
  , TypeFamilies
  , DeriveGeneric
  , NamedFieldPuns
  , FlexibleContexts
  , FlexibleInstances
  , StandaloneDeriving
  , ScopedTypeVariables
  , UndecidableInstances
  , MultiParamTypeClasses
  , ExistentialQuantification
  #-}

{-|

Module: Test.Serialization.Symbiote
Copyright: (c) 2019 Athan Clark
License: BSD-3-Style
Maintainer: athan.clark@gmail.com
Portability: GHC

As an example, say you have some data type @TypeA@, and some encoding / decoding instance with Aeson
for that data type. Now, you've also got a few functions that work with that data type - @f :: TypeA -> TypeA@
and @g :: TypeA -> TypeA -> TypeA@, and you've also taken the time to write a proper 'Arbitrary' instance for @TypeA@.

Your first order of business in making @TypeA@ a symbiote, is to first demonstrate what operations are supported by it:

> {-# LANGUAGE MultiparamTypeClasses, TypeFamilies #-}
>
> instance SymbioteOperation TypeA where
>   data Operation TypeA
>     = F
>     | G TypeA
>   perform op x = case op of
>     F -> f x
>     G y -> g y x

You're also going to need to make sure your new data-family has appropriate serialization instances, as well:

> instance ToJSON (Operation TypeA) where
>   toJSON op = case op of
>     F -> toJSON "f"
>     G x -> "g" .: x
>
> instance FromJSON (Operation TypeA) where
>   parseJSON json = getF <|> getG
>     where
>       getF = do
>         s <- parseJSON json
>         if s == "f"
>           then pure F
>           else fail "Not F"
>       getG = do
>         x <- json .: "g"
>         pure (G x)

Next, let's make @TypeA@ an instance of 'Symbiote':

> instance Symbiote TypeA Value where
>   encode = Aeson.toJSON
>   decode = Aeson.parseMaybe Aeson.parseJSON
>   encodeOp = Aeson.toJSON
>   decodeOp = Aeson.parseMaybe Aeson.parseJSON

this instance above actually works for any type that implements @ToJSON@ and @FromJSON@ - there's an orphan
definition in "Test.Serialization.Symbiote.Aeson".

Next, you're going to need to actually use this, by registering the type in a test suite:

> myFancyTestSuite :: SymbioteT Value IO ()
> myFancyTestSuite = register "TypeA" 100 (Proxy :: Proxy TypeA)

Lastly, you're going to need to actually run the test suite by attaching it to a network. The best way to
do that, is decide whether this peer will be the first or second peer to start the protocol, then use the
respective 'firstPeer' and 'secondPeer' functions - they take as arguments functions that define "how to send"
and "how to receive" messages, and likewise how to report status.

-}

module Test.Serialization.Symbiote
  ( SymbioteOperation (..), Symbiote (..), SimpleSerialization (..), Topic, SymbioteT, register
  , firstPeer, secondPeer, First (..), Second (..), Generating (..), Operating (..), Failure (..)
  , defaultSuccess, defaultFailure, defaultProgress, nullProgress, simpleTest, simpleTest'
  ) where

import Test.Serialization.Symbiote.Core
  ( Topic (..), newGeneration, SymbioteState (..), ExistsSymbiote (..), SymbioteT, runSymbioteT
  , GenerateSymbiote (..), generateSymbiote, getProgress, Symbiote (..), SymbioteOperation (..))

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (unpack)
import Data.Proxy (Proxy (..))
import Text.Printf (printf)
import Control.Concurrent.STM
  (TVar, newTVarIO, readTVarIO, writeTVar, atomically, newTChan, readTChan, writeTChan)
import Control.Concurrent.Async (async, wait)
import Control.Monad (void)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseWith)
import Control.Monad.State (modify')
import Control.Monad.IO.Class (MonadIO, liftIO)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import GHC.Generics (Generic)


-- | The most trivial serialization medium for any @a@ and @o@.
data SimpleSerialization a o
  = SimpleValue a
  | SimpleOutput o
  | SimpleOperation (Operation a)
  deriving (Generic)
deriving instance (Show a, Show o, Show (Operation a)) => Show (SimpleSerialization a o)
deriving instance (Eq a, Eq o, Eq (Operation a)) => Eq (SimpleSerialization a o)

instance SymbioteOperation a o => Symbiote a o (SimpleSerialization a o) where
  encode = SimpleValue
  decode (SimpleValue x) = Just x
  decode _ = Nothing
  encodeOut _ = SimpleOutput
  decodeOut _ (SimpleOutput x) = Just x
  decodeOut _ _ = Nothing
  encodeOp = SimpleOperation
  decodeOp (SimpleOperation x) = Just x
  decodeOp _ = Nothing


-- | Register a topic in the test suite
register :: forall a o s m
          . Arbitrary a
         => Arbitrary (Operation a)
         => Symbiote a o s
         => Eq o
         => MonadIO m
         => Topic
         -> Int -- ^ Max size
         -> Proxy a -- ^ Reference to datatype
         -> SymbioteT s m ()
register t maxSize Proxy = do
  generation <- liftIO (newTVarIO newGeneration)
  let newState :: SymbioteState a o s
      newState = SymbioteState
        { generate = arbitrary :: Gen a
        , generateOp = arbitrary :: Gen (Operation a)
        , equal = (==) :: o -> o -> Bool
        , maxSize
        , generation
        , encode'    = encode
        , encodeOut' = encodeOut (Proxy :: Proxy a)
        , encodeOp'  = encodeOp
        , decode'    = decode
        , decodeOut' = decodeOut (Proxy :: Proxy a)
        , decodeOp'  = decodeOp
        , perform'   = perform
        }
  modify' (Map.insert t (ExistsSymbiote newState))

-- | Messages sent by a peer during their generating phase
data Generating s
  = Generated
    { genValue :: s
    , genOperation :: s
    }
  | BadResult s -- ^ Expected value
  | YourTurn
  | ImFinished
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
  = BadTopics (Map Topic Int)
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
defaultProgress (Topic t) p = putStrLn $ "Topic " ++ unpack t ++ ": " ++ printf "%.2f" (p * 100.0) ++ "%"

-- | Do nothing
nullProgress :: Applicative m => Topic -> Float -> m ()
nullProgress _ _ = pure ()


-- | Run the test suite as the first peer
firstPeer :: forall m s
           . MonadIO m
          => Show s
          => (First s -> m ()) -- ^ Encode and send first messages
          -> m (Second s) -- ^ Receive and decode second messages
          -> (Topic -> m ()) -- ^ Report when Successful
          -> (Failure Second s -> m ()) -- ^ Report when Failed
          -> (Topic -> Float -> m ()) -- ^ Report on Progress
          -> SymbioteT s m ()
          -> m ()
firstPeer encodeAndSend receiveAndDecode onSuccess onFailure onProgress x = do
  state <- runSymbioteT x True
  let topics = go <$> state
        where
          go s = case s of
            ExistsSymbiote s' -> maxSize s'
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


-- | Run the test suite as the second peer
secondPeer :: forall s m
            . MonadIO m
           => Show s
           => (Second s -> m ()) -- ^ Encode and send second messages
           -> m (First s) -- ^ Receive and decode first messages
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
      let myTopics = go <$> state
            where
              go s = case s of
                ExistsSymbiote s' -> maxSize s'
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


generating :: forall s m them me
            . MonadIO m
           => Show s
           => (me s -> m ()) -- ^ Encode and send first messages
           -> m (them s) -- ^ Receive and decode second messages
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
           -> ExistsSymbiote s
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
  topic existsSymbiote = do -- symbioteState@SymbioteState{equal,encodeOut'} = do
  mGenerated <- generateSymbiote existsSymbiote -- symbioteState
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
              Operated operatedValueEncoded -> case existsSymbiote of
                ExistsSymbiote symbioteState ->
                  let go :: forall a o
                          . Arbitrary a
                         => Arbitrary (Operation a)
                         => Symbiote a o s
                         => Eq o
                         => SymbioteState a o s -> m ()
                      go SymbioteState{decode',decodeOp',decodeOut',equal,encodeOut',perform'} = case decodeOut' operatedValueEncoded of
                        Nothing -> do
                          encodeAndSend $ makeGen topic $ GeneratingNoParseOperated operatedValueEncoded
                          onFailure $ CantParseOperated topic operatedValueEncoded
                        Just operatedValue -> case decode' generatedValueEncoded of
                          Nothing -> onFailure $ CantParseLocalValue topic generatedValueEncoded
                          Just generatedValue -> case decodeOp' generatedOperationEncoded of
                            Nothing -> onFailure $ CantParseLocalOperation topic generatedOperationEncoded
                            Just generatedOperation -> do
                              -- decoded operated value, generated value & operation
                              let expected :: o
                                  expected = perform' generatedOperation generatedValue
                              if  equal expected operatedValue
                                then do
                                  encodeAndSend $ makeGen topic YourTurn
                                  progress <- getProgress existsSymbiote
                                  onProgress topic progress
                                  operating
                                    encodeAndSend receiveAndDecode
                                    makeGen makeOp
                                    getGen getOp
                                    hasSentFinishedVar hasReceivedFinishedVar
                                    onFinished
                                    onSuccess
                                    onFailure
                                    onProgress
                                    topic existsSymbiote
                                else do
                                  encodeAndSend $ makeGen topic $ BadResult operatedValueEncoded
                                  onFailure $ SafeFailure topic (encodeOut' expected) operatedValueEncoded
                  in  go symbioteState
              _ -> onFailure $ BadOperating topic shouldBeOperated
        _ -> onFailure $ BadThem topic shouldBeOperating
  where

    operatingTryFinished :: m ()
    operatingTryFinished = do
      hasReceivedFinished <- liftIO $ readTVarIO hasReceivedFinishedVar
      case hasReceivedFinished of
        HasReceivedFinished -> do
          onSuccess topic
          onFinished -- stop cycling - last generation in sequence is from second
        HasntReceivedFinished -> do
          progress <- getProgress existsSymbiote
          onProgress topic progress
          operating
            encodeAndSend receiveAndDecode
            makeGen makeOp
            getGen getOp
            hasSentFinishedVar hasReceivedFinishedVar
            onFinished
            onSuccess
            onFailure
            onProgress
            topic existsSymbiote

operating :: forall s m them me
           . MonadIO m
          => Show s
          => (me s -> m ()) -- ^ Encode and send first messages
          -> m (them s) -- ^ Receive and decode second messages
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
          -> ExistsSymbiote s
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
  topic existsSymbiote = do -- symbioteState@SymbioteState{decode',decodeOp',perform',encode'} = do
  shouldBeGenerating <- receiveAndDecode
  case getGen shouldBeGenerating of
    Just (secondGeneratingTopic,shouldBeGenerated)
      | secondGeneratingTopic /= topic ->
        onFailure $ WrongTopic topic secondGeneratingTopic
      | otherwise -> case existsSymbiote of
          ExistsSymbiote symbioteState -> go symbioteState shouldBeGenerated -- case shouldBeGenerated of
    _ -> onFailure $ BadThem topic shouldBeGenerating
  where
    go :: forall a o
        . Arbitrary a
       => Arbitrary (Operation a)
       => Symbiote a o s
       => Eq o
       => SymbioteState a o s -> Generating s -> m ()
    go SymbioteState{decode',decodeOp',perform',encodeOut'} shouldBeGenerated = case shouldBeGenerated of
      ImFinished -> do
        liftIO $ atomically $ writeTVar hasReceivedFinishedVar HasReceivedFinished
        generatingTryFinished
      YourTurn -> do
        progress <- getProgress existsSymbiote
        onProgress topic progress
        generating
          encodeAndSend receiveAndDecode
          makeGen makeOp
          getGen getOp
          hasSentFinishedVar hasReceivedFinishedVar
          onFinished
          onSuccess
          onFailure
          onProgress
          topic existsSymbiote
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
            encodeAndSend $ makeOp topic $ Operated $ encodeOut' $ perform' generatedOperation generatedValue
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
              topic existsSymbiote
      _ -> onFailure $ BadGenerating topic shouldBeGenerated

    generatingTryFinished :: m ()
    generatingTryFinished = do
      hasSentFinished <- liftIO $ readTVarIO hasSentFinishedVar
      case hasSentFinished of
        HasSentFinished -> do
          onSuccess topic
          onFinished -- stop cycling - last operation in sequence is from first
        HasntSentFinished -> do
          progress <- getProgress existsSymbiote
          onProgress topic progress
          generating
            encodeAndSend receiveAndDecode
            makeGen makeOp
            getGen getOp
            hasSentFinishedVar hasReceivedFinishedVar
            onFinished
            onSuccess
            onFailure
            onProgress
            topic existsSymbiote


-- | Prints to stdout and uses a local channel for a sanity-check - doesn't serialize.
simpleTest :: MonadBaseControl IO m
           => MonadIO m
           => Show s
           => SymbioteT s m () -> m ()
simpleTest =
  simpleTest'
    (const (pure ()))
    (liftIO . defaultFailure)
    (liftIO . defaultFailure)
    nullProgress

simpleTest' :: MonadBaseControl IO m
            => MonadIO m
            => Show s
            => (Topic -> m ()) -- ^ report topic success
            -> (Failure Second s -> m ()) -- ^ report topic failure from first (sees second)
            -> (Failure First s -> m ()) -- ^ report topic failure from second (sees first)
            -> (Topic -> Float -> m ()) -- ^ report topic progress
            -> SymbioteT s m () -> m ()
simpleTest' onSuccess onFailureSecond onFailureFirst onProgress suite = do
  firstChan <- liftIO $ atomically newTChan
  secondChan <- liftIO $ atomically newTChan

  t <- liftBaseWith $ \runInBase -> async $
    void $ runInBase $ firstPeer
      (encodeAndSendChan firstChan)
      (receiveAndDecodeChan secondChan)
      onSuccess onFailureSecond onProgress
      suite
  secondPeer
    (encodeAndSendChan secondChan)
    (receiveAndDecodeChan firstChan)
    onSuccess onFailureFirst onProgress
    suite
  liftIO (wait t)
  where
    encodeAndSendChan chan x = liftIO $ atomically (writeTChan chan x)
    receiveAndDecodeChan chan = liftIO $ atomically (readTChan chan)
