{-# LANGUAGE
    RankNTypes
  , TypeFamilies
  , DeriveGeneric
  , NamedFieldPuns
  , RecordWildCards
  , FlexibleContexts
  , FlexibleInstances
  , OverloadedStrings
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
> instance SymbioteOperation TypeA TypeA where
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
>           else typeMismatch "Operation TypeA" json
>       getG = do
>         o <- parseJSON json
>         G <$> o .: "g"

Next, let's make @TypeA@ an instance of 'Symbiote':

> instance Symbiote TypeA TypeA Value where
>   encode = Aeson.toJSON
>   decode = Aeson.parseMaybe Aeson.parseJSON
>   encodeOut _ = Aeson.toJSON
>   decodeOut _ = Aeson.parseMaybe Aeson.parseJSON
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
  ( -- * Suite Building
    SymbioteOperation (..), Symbiote (..), SimpleSerialization (..), Topic, SymbioteT, register
  , -- * Protocol Messages
    First (..), Second (..), Generating (..), Operating (..)
  , -- * Result Handling
    Failure (..), defaultSuccess, defaultFailure, defaultProgress, nullProgress
  , -- * Test Execution
    simpleTest, simpleTest', firstPeer, secondPeer
  ) where

import Test.Serialization.Symbiote.Core
  ( Topic (..), newGeneration, SymbioteState (..), ExistsSymbiote (..), SymbioteT, runSymbioteT
  , GenerateSymbiote (..), generateSymbiote, getProgress, Symbiote (..), SymbioteOperation (..))

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int32)
import Data.Text (unpack)
import Data.Proxy (Proxy (..))
import Data.Aeson (ToJSON (..), FromJSON (..), (.=), object, (.:), Value (Object, String))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (..))
import Data.Serialize.Put (putWord8, putInt32be, putByteString, putLazyByteString, PutM)
import Data.Serialize.Get (getWord8, getInt32be, getByteString, getLazyByteString, Get)
import Text.Printf (printf)
import Control.Concurrent.STM
  (TVar, newTVarIO, readTVarIO, writeTVar, atomically, newTChan, readTChan, writeTChan)
import Control.Concurrent.Async (async, wait)
import Control.Applicative ((<|>))
import Control.Monad (void, replicateM)
import Control.Monad.Trans.Control.Aligned (MonadBaseControl, liftBaseWith)
import Control.Monad.State (modify')
import Control.Monad.IO.Class (MonadIO, liftIO)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, oneof)
import GHC.Generics (Generic)


-- | The most trivial serialization medium for any @a@ and @o@.
-- There's no need to implement transmission protocol specific instances for this type, like 'ToJSON' or 'Serialize', because it is intended to operate locally (on the same program), and over 'Eq'.
data SimpleSerialization a o
  = -- | A value @a@ encodes as just that value
    SimpleValue a
  | -- | An output @o@ encodes as just that output
    SimpleOutput o
  | -- | An operation 'Operation' encodes as just that operation
    SimpleOperation (Operation a)
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


-- | Register a topic in the test suite builder.
register :: forall a o s m
          . Arbitrary a
         => Arbitrary (Operation a)
         => Symbiote a o s
         => Eq o
         => MonadIO m
         => Topic -- ^ Topic name as a 'Text'
         -> Int32 -- ^ Max size
         -> Proxy a -- ^ Reference to the datatype
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


---------------- Protocol Messages -----------------------


-- | Messages sent by a peer during their generating phase - polymorphic in the serialization medium.
data Generating s
  = -- | \"I\'ve generated a value and operation, here you go.\"
    Generated
    { genValue :: s
    , genOperation :: s
    }
  | -- | \"You sent the wrong value!\"
    BadResult s
  | -- | \"It\'s your turn to generate, I just finished and we\'re both O.K.\"
    YourTurn
  | -- | \"I just finished all generation, and my topic state\'s 'size' is equal to the 'maxSize'.\"
    ImFinished
  | -- | \"I could not deserialize the output value sent by you, and I have to tell you about it.\"
    GeneratingNoParseOperated s
  deriving (Eq, Show, Generic)
instance Arbitrary s => Arbitrary (Generating s) where
  arbitrary = oneof
    [ Generated <$> arbitrary <*> arbitrary
    , BadResult <$> arbitrary
    , pure YourTurn
    , pure ImFinished
    , GeneratingNoParseOperated <$> arbitrary
    ]
instance ToJSON s => ToJSON (Generating s) where
  toJSON x = case x of
    Generated{..} -> object ["generated" .= object ["value" .= genValue, "operation" .= genOperation]]
    BadResult r -> object ["badResult" .= r]
    YourTurn -> String "yourTurn"
    ImFinished -> String "imFinished"
    GeneratingNoParseOperated r -> object ["noParseOperated" .= r]
instance FromJSON s => FromJSON (Generating s) where
  parseJSON (Object o) = generated <|> badResult <|> noParseOperated
    where
      generated = do
        o' <- o .: "generated"
        Generated <$> o' .: "value" <*> o' .: "operation"
      badResult = BadResult <$> o .: "badResult"
      noParseOperated = GeneratingNoParseOperated <$> o .: "noParseOperated"
  parseJSON x@(String s)
    | s == "imFinished" = pure ImFinished
    | s == "yourTurn" = pure YourTurn
    | otherwise = typeMismatch "Generating s" x
  parseJSON x = typeMismatch "Generating s" x
-- | For supporting 32bit limitation on length prefix
instance Serialize (Generating BS.ByteString) where
  put x = case x of
    Generated{..} -> putWord8 0 *> putByteString' genValue *> putByteString' genOperation
    BadResult r -> putWord8 1 *> putByteString' r
    YourTurn -> putWord8 2
    ImFinished -> putWord8 3
    GeneratingNoParseOperated r -> putWord8 4 *> putByteString' r
  get = do
    x <- getWord8
    case x of
      0 -> Generated <$> getByteString' <*> getByteString'
      1 -> BadResult <$> getByteString'
      2 -> pure YourTurn
      3 -> pure ImFinished
      4 -> GeneratingNoParseOperated <$> getByteString'
      _ -> fail "Generating ByteString"
-- | For supporting 32bit limitation on length prefix
instance Serialize (Generating LBS.ByteString) where
  put x = case x of
    Generated{..} -> putWord8 0 *> putLazyByteString' genValue *> putLazyByteString' genOperation
    BadResult r -> putWord8 1 *> putLazyByteString' r
    YourTurn -> putWord8 2
    ImFinished -> putWord8 3
    GeneratingNoParseOperated r -> putWord8 4 *> putLazyByteString' r
  get = do
    x <- getWord8
    case x of
      0 -> Generated <$> getLazyByteString' <*> getLazyByteString'
      1 -> BadResult <$> getLazyByteString'
      2 -> pure YourTurn
      3 -> pure ImFinished
      4 -> GeneratingNoParseOperated <$> getLazyByteString'
      _ -> fail "Generating LazyByteString"

-- | Messages sent by a peer during their operating phase - polymorphic in the serialization medium.
data Operating s
  = -- | \"I\'ve performed the operation on the value, and here's the output result.\"
    Operated s
  | -- | \"I couldn't deserialize the input value sent by you, and I have to tell you about it.\"
    OperatingNoParseValue s
  | -- | \"I couldn't deserialize the operation sent by you, and I have to tell you about it.\"
    OperatingNoParseOperation s
  deriving (Eq, Show, Generic)
instance Arbitrary s => Arbitrary (Operating s) where
  arbitrary = oneof
    [ Operated <$> arbitrary
    , OperatingNoParseValue <$> arbitrary
    , OperatingNoParseOperation <$> arbitrary
    ]
instance ToJSON s => ToJSON (Operating s) where
  toJSON x = case x of
    Operated r -> object ["operated" .= r]
    OperatingNoParseValue r -> object ["noParseValue" .= r]
    OperatingNoParseOperation r -> object ["noParseOperation" .= r]
instance FromJSON s => FromJSON (Operating s) where
  parseJSON (Object o) = operated <|> noParseValue <|> noParseOperation
    where
      operated = Operated <$> o .: "operated"
      noParseValue = OperatingNoParseValue <$> o .: "noParseValue"
      noParseOperation = OperatingNoParseOperation <$> o .: "noParseOperation"
  parseJSON x = typeMismatch "Operating s" x
-- | For supporting 32bit limitation on length prefix
instance Serialize (Operating BS.ByteString) where
  put x = case x of
    Operated y -> putWord8 0 *> putByteString' y
    OperatingNoParseValue r -> putWord8 1 *> putByteString' r
    OperatingNoParseOperation r -> putWord8 2 *> putByteString' r
  get = do
    x <- getWord8
    case x of
      0 -> Operated <$> getByteString'
      1 -> OperatingNoParseValue <$> getByteString'
      2 -> OperatingNoParseOperation <$> getByteString'
      _ -> fail "Operating ByteString"
-- | For supporting 32bit limitation on length prefix
instance Serialize (Operating LBS.ByteString) where
  put x = case x of
    Operated y -> putWord8 0 *> putLazyByteString' y
    OperatingNoParseValue r -> putWord8 1 *> putLazyByteString' r
    OperatingNoParseOperation r -> putWord8 2 *> putLazyByteString' r
  get = do
    x <- getWord8
    case x of
      0 -> Operated <$> getLazyByteString'
      1 -> OperatingNoParseValue <$> getLazyByteString'
      2 -> OperatingNoParseOperation <$> getLazyByteString'
      _ -> fail "Operating LazyByteString"

-- | Messages sent by the first peer - polymorphic in the serialization medium.
data First s
  = -- | \"Here are the topics I support.\"
    AvailableTopics (Map Topic Int32)
  | -- | \"I got your subset of topics, but they don\'t match to mine.\"
    BadStartSubset
  | -- | \"It\'s my turn to generate, so here\'s a generating message.\"
    FirstGenerating
    { firstGeneratingTopic :: Topic
    , firstGenerating :: Generating s
    }
  | -- | \"It's my turn to operate, so here\'s my operating message.\"
    FirstOperating
    { firstOperatingTopic :: Topic
    , firstOperating :: Operating s
    }
  deriving (Eq, Show, Generic)
instance Arbitrary s => Arbitrary (First s) where
  arbitrary = oneof
    [ AvailableTopics <$> arbitrary
    , pure BadStartSubset
    , FirstGenerating <$> arbitrary <*> arbitrary
    , FirstOperating <$> arbitrary <*> arbitrary
    ]
instance ToJSON s => ToJSON (First s) where
  toJSON x = case x of
    AvailableTopics ts -> object ["availableTopics" .= ts]
    BadStartSubset -> String "badStartSubset"
    FirstGenerating t y -> object ["firstGenerating" .= object ["topic" .= t, "generating" .= y]]
    FirstOperating t y -> object ["firstOperating" .= object ["topic" .= t, "operating" .= y]]
instance FromJSON s => FromJSON (First s) where
  parseJSON json = case json of
    Object o ->
      let availableTopics = AvailableTopics <$> o .: "availableTopics"
          firstGenerating' = do
            o' <- o .: "firstGenerating"
            FirstGenerating <$> o' .: "topic" <*> o' .: "generating"
          firstOperating' = do
            o' <- o .: "firstOperating"
            FirstOperating <$> o' .: "topic" <*> o' .: "operating"
      in  availableTopics <|> firstGenerating' <|> firstOperating'
    String s
      | s == "badStartSubset" -> pure BadStartSubset
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "First s" json
instance Serialize (First BS.ByteString) where
  put x = case x of
    AvailableTopics ts -> do
      putWord8 0
      let ls = Map.toList ts
      putInt32be (fromIntegral (length ls))
      void (traverse put ls)
    BadStartSubset -> putWord8 1
    FirstGenerating t y -> putWord8 2 *> put t *> put y
    FirstOperating t y -> putWord8 3 *> put t *> put y
  get = do
    x <- getWord8
    case x of
      0 -> do
        l <- getInt32be
        AvailableTopics . Map.fromList <$> replicateM (fromIntegral l) get
      1 -> pure BadStartSubset
      2 -> FirstGenerating <$> get <*> get
      3 -> FirstOperating <$> get <*> get
      _ -> fail "First BS.ByteString"
instance Serialize (First LBS.ByteString) where
  put x = case x of
    AvailableTopics ts -> do
      putWord8 0
      let ls = Map.toList ts
      putInt32be (fromIntegral (length ls))
      void (traverse put ls)
    BadStartSubset -> putWord8 1
    FirstGenerating t y -> putWord8 2 *> put t *> put y
    FirstOperating t y -> putWord8 3 *> put t *> put y
  get = do
    x <- getWord8
    case x of
      0 -> do
        l <- getInt32be
        AvailableTopics . Map.fromList <$> replicateM (fromIntegral l) get
      1 -> pure BadStartSubset
      2 -> FirstGenerating <$> get <*> get
      3 -> FirstOperating <$> get <*> get
      _ -> fail "First LBS.ByteString"

-- | Convenience function for avoiding a case match over First
getFirstGenerating :: First s -> Maybe (Topic, Generating s)
getFirstGenerating x = case x of
  FirstGenerating topic g -> Just (topic, g)
  _ -> Nothing

-- | Convenience function for avoiding a case match over First
getFirstOperating :: First s -> Maybe (Topic, Operating s)
getFirstOperating x = case x of
  FirstOperating topic g -> Just (topic, g)
  _ -> Nothing


-- | Messages sent by the second peer - polymorphic in the serialization medium.
data Second s
  = -- | \"Although my topics should be at least a subset of your topics available,
    -- the following of mine do not have the same max size as yours.\"
    BadTopics (Map Topic Int32)
  | -- | \"All systems nominal, you may fire (the following subset of topics) when ready.\"
    Start (Set Topic)
  | -- | \"It's my turn to operate, so here\'s my operating message.\"
    SecondOperating
    { secondOperatingTopic :: Topic
    , secondOperating :: Operating s
    }
  | -- | \"It's my turn to generate, so here\'s my generating message.\"
    SecondGenerating
    { secondGeneratingTopic :: Topic
    , secondGenerating :: Generating s
    }
  deriving (Eq, Show, Generic)
instance Arbitrary s => Arbitrary (Second s) where
  arbitrary = oneof
    [ BadTopics <$> arbitrary
    , Start <$> arbitrary
    , SecondOperating <$> arbitrary <*> arbitrary
    , SecondGenerating <$> arbitrary <*> arbitrary
    ]
instance ToJSON s => ToJSON (Second s) where
  toJSON x = case x of
    BadTopics ts -> object ["badTopics" .= ts]
    Start ts -> object ["start" .= ts]
    SecondOperating t y -> object ["secondOperating" .= object ["topic" .= t, "operating" .= y]]
    SecondGenerating t y -> object ["secondGenerating" .= object ["topic" .= t, "generating" .= y]]
instance FromJSON s => FromJSON (Second s) where
  parseJSON (Object o) = badTopics <|> start <|> secondOperating' <|> secondGenerating'
    where
      badTopics = BadTopics <$> o .: "badTopics"
      start = Start <$> o .: "start"
      secondOperating' = do
        o' <- o .: "secondOperating"
        SecondOperating <$> o' .: "topic" <*> o' .: "operating"
      secondGenerating' = do
        o' <- o .: "secondGenerating"
        SecondGenerating <$> o' .: "topic" <*> o' .: "generating"
  parseJSON x = typeMismatch "Second s" x
instance Serialize (Second BS.ByteString) where
  put x = case x of
    BadTopics ts -> do
      putWord8 0
      let ls = Map.toList ts
      putInt32be (fromIntegral (length ls))
      void (traverse put ls)
    Start ts -> do
      putWord8 1
      let ls = Set.toList ts
      putInt32be (fromIntegral (length ls))
      void (traverse put ls)
    SecondOperating t y -> putWord8 2 *> put t *> put y
    SecondGenerating t y -> putWord8 3 *> put t *> put y
  get = do
    x <- getWord8
    case x of
      0 -> do
        l <- getInt32be
        BadTopics . Map.fromList <$> replicateM (fromIntegral l) get
      1 -> do
        l <- getInt32be
        Start . Set.fromList <$> replicateM (fromIntegral l) get
      2 -> SecondOperating <$> get <*> get
      3 -> SecondGenerating <$> get <*> get
      _ -> fail "Second BS.ByteString"
instance Serialize (Second LBS.ByteString) where
  put x = case x of
    BadTopics ts -> do
      putWord8 0
      let ls = Map.toList ts
      putInt32be (fromIntegral (length ls))
      void (traverse put ls)
    Start ts -> do
      putWord8 1
      let ls = Set.toList ts
      putInt32be (fromIntegral (length ls))
      void (traverse put ls)
    SecondOperating t y -> putWord8 2 *> put t *> put y
    SecondGenerating t y -> putWord8 3 *> put t *> put y
  get = do
    x <- getWord8
    case x of
      0 -> do
        l <- getInt32be
        BadTopics . Map.fromList <$> replicateM (fromIntegral l) get
      1 -> do
        l <- getInt32be
        Start . Set.fromList <$> replicateM (fromIntegral l) get
      2 -> SecondOperating <$> get <*> get
      3 -> SecondGenerating <$> get <*> get
      _ -> fail "Second LBS.ByteString"

-- | Convenience function for avoiding a case match over First
getSecondGenerating :: Second s -> Maybe (Topic, Generating s)
getSecondGenerating x = case x of
  SecondGenerating topic g -> Just (topic, g)
  _ -> Nothing

-- | Convenience function for avoiding a case match over First
getSecondOperating :: Second s -> Maybe (Topic, Operating s)
getSecondOperating x = case x of
  SecondOperating topic g -> Just (topic, g)
  _ -> Nothing


-- | Exception data type
data Failure them s
  = -- | Topic sets do not match between peers
    BadTopicsFailure
    { badTopicsFirst :: Map Topic Int32
    , badTopicsSecond :: Map Topic Int32
    }
  | -- | The first peer doesn\'t have the subset topics identified by second
    BadStartSubsetFailure (Set Topic)
  | -- | The first peer is out of sync and not sending the correct message
    OutOfSyncFirst (First s)
  | -- | The second peer is out of sync and not sending the correct message
    OutOfSyncSecond (Second s)
  | -- | Topic does not exist
    TopicNonexistent Topic
  | -- | Got the wrong topic
    WrongTopic
    { wrongTopicExpected :: Topic
    , wrongTopicGot :: Topic
    }
  | CantParseOperated Topic s
  | CantParseGeneratedValue Topic s
  | CantParseGeneratedOperation Topic s
  | CantParseLocalValue Topic s
  | CantParseLocalOperation Topic s
  | -- | Incorrect operating message received
    BadOperating Topic (Operating s)
  | -- | Incorrect generating message received
    BadGenerating Topic (Generating s)
  | -- | Incorrect peer message ('First' and 'Second' agnostic)
    BadThem Topic (them s)
  | -- | Failed because the output of the operation applied to the value does not match between the peers.
    SafeFailure
    { safeFailureTopic :: Topic
    , safeFailureExpected :: s
    , safeFailureGot :: s
    }
  deriving (Eq, Show)


-- | Via 'putStrLn'
defaultSuccess :: Topic -> IO ()
defaultSuccess (Topic t) = putStrLn $ "Topic " ++ unpack t ++ " succeeded"

-- | Via 'putStrLn'
defaultFailure :: Show (them s) => Show s => Failure them s -> IO ()
defaultFailure f = error $ "Failure: " ++ show f

-- | Via 'putStrLn'
defaultProgress :: Topic -> Float -> IO ()
defaultProgress (Topic t) p = putStrLn $ "Topic " ++ unpack t ++ ": " ++ printf "%.2f" (p * 100.0) ++ "%"

-- | Do nothing
nullProgress :: Applicative m => Topic -> Float -> m ()
nullProgress _ _ = pure ()


-- | Run the test suite as the first peer - see "Test.Serialization.Symbiote.WebSocket" and "Test.Serialization.Symbiote.ZeroMQ" for end-user
-- implementations.
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
  let myTopics = go <$> state
        where
          go s = case s of
            ExistsSymbiote s' -> maxSize s'
  encodeAndSend (AvailableTopics myTopics)
  shouldBeStart <- receiveAndDecode
  case shouldBeStart of
    BadTopics badTopics -> onFailure (BadTopicsFailure myTopics badTopics)
    Start topicsSubset
      | not (topicsSubset `Set.isSubsetOf` Map.keysSet myTopics) -> do
        encodeAndSend BadStartSubset
        onFailure (BadStartSubsetFailure topicsSubset)
      | otherwise -> do
        topicsToProcess <- liftIO (newTVarIO topicsSubset)
        let processAllTopics = do
              mTopicToProcess <- Set.maxView <$> liftIO (readTVarIO topicsToProcess)
              case mTopicToProcess of
                Nothing -> pure () -- done
                Just (topic, newTopics) -> do
                  liftIO (atomically (writeTVar topicsToProcess newTopics))
                  case Map.lookup topic state of
                    Nothing -> onFailure (TopicNonexistent topic)
                    Just symbioteState -> do
                      hasSentFinishedVar <- liftIO (newTVarIO HasntSentFinished)
                      hasReceivedFinishedVar <- liftIO (newTVarIO HasntReceivedFinished)
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
    _ -> onFailure (OutOfSyncSecond shouldBeStart)


-- | Run the test suite as the second peer - see "Test.Serialization.Symbiote.WebSocket" and "Test.Serialization.Symbiote.ZeroMQ" for end-user
-- implementations.
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
    AvailableTopics topicsAvailable -> do
      let myTopics = go <$> state
            where
              go s = case s of
                ExistsSymbiote s' -> maxSize s'
      if not (myTopics `Map.isSubmapOf` topicsAvailable)
        then do
          encodeAndSend (BadTopics myTopics)
          onFailure (BadTopicsFailure topicsAvailable myTopics)
        else do
          encodeAndSend (Start (Map.keysSet myTopics))
          topicsToProcess <- liftIO (newTVarIO (Map.keysSet myTopics))
          let processAllTopics = do
                mTopicToProcess <- Set.maxView <$> liftIO (readTVarIO topicsToProcess)
                case mTopicToProcess of
                  Nothing -> pure () -- done
                  Just (topic, newTopics) -> do
                    liftIO (atomically (writeTVar topicsToProcess newTopics))
                    case Map.lookup topic state of
                      Nothing -> onFailure (TopicNonexistent topic)
                      Just symbioteState -> do
                        hasSentFinishedVar <- liftIO (newTVarIO HasntSentFinished)
                        hasReceivedFinishedVar <- liftIO (newTVarIO HasntReceivedFinished)
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
    _ -> onFailure (OutOfSyncFirst shouldBeAvailableTopics)


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
simpleTest :: MonadBaseControl IO m stM
           => MonadIO m
           => Show s
           => SymbioteT s m () -> m ()
simpleTest =
  simpleTest'
    (const (pure ()))
    (liftIO . defaultFailure)
    (liftIO . defaultFailure)
    nullProgress

simpleTest' :: MonadBaseControl IO m stM
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

putByteString' :: BS.ByteString -> PutM ()
putByteString' b = do
  putInt32be (fromIntegral (BS.length b))
  putByteString b

getByteString' :: Get BS.ByteString
getByteString' = do
  l <- getInt32be
  getByteString (fromIntegral l)

putLazyByteString' :: LBS.ByteString -> PutM ()
putLazyByteString' b = do
  putInt32be (fromIntegral (LBS.length b))
  putLazyByteString b

getLazyByteString' :: Get LBS.ByteString
getLazyByteString' = do
  l <- getInt32be
  getLazyByteString (fromIntegral l)
