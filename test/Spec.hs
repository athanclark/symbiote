{-# LANGUAGE
    OverloadedStrings
  , MultiParamTypeClasses
  , TypeFamilies
  , TypeSynonymInstances
  , FlexibleInstances
  , StandaloneDeriving
  #-}

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase)
import Test.Serialization.Symbiote
  ( SymbioteT, register, firstPeer, secondPeer, SymbioteOperation (..), Symbiote (..), EitherOp
  , First, Second, defaultSuccess, nullProgress, defaultFailure)
import Test.QuickCheck (Arbitrary (..))

import Data.Proxy (Proxy (..))
import Control.Concurrent.STM (TChan, newTChan, readTChan, writeTChan, atomically)
import Control.Concurrent.Async (async, wait)


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "All Tests"
  [ testGroup "Simple Tests"
    [ testCase "Unit over id" unitOverId
    ]
  ]
  where
    unitOverId :: IO ()
    unitOverId = do
      firstChan <- atomically newTChan
      secondChan <- atomically newTChan

      t <- async $
        firstPeer
          (encodeAndSendChanFirst firstChan)
          (receiveAndDecodeChanSecond secondChan)
          defaultSuccess defaultFailure nullProgress
          suite
      secondPeer
        (encodeAndSendChanSecond secondChan)
        (receiveAndDecodeChanFirst firstChan)
        defaultSuccess defaultFailure nullProgress
        suite
      wait t

      where
        suite :: SymbioteT (EitherOp ()) IO ()
        suite = register "Unit" 100 (Proxy :: Proxy ())


encodeAndSendChanFirst :: Show s => TChan (First s) -> First s -> IO ()
encodeAndSendChanFirst chan x = atomically (writeTChan chan x)

encodeAndSendChanSecond :: Show s => TChan (Second s) -> Second s -> IO ()
encodeAndSendChanSecond chan x = atomically (writeTChan chan x)

receiveAndDecodeChanFirst :: Show s => TChan (First s) -> IO (First s)
receiveAndDecodeChanFirst chan = atomically (readTChan chan)

receiveAndDecodeChanSecond :: Show s => TChan (Second s) -> IO (Second s)
receiveAndDecodeChanSecond chan = atomically (readTChan chan)



instance SymbioteOperation () where
  data Operation () = UnitId
  perform UnitId () = ()

instance Symbiote () (EitherOp ()) where
  encode = Left
  decode (Left x) = Just x
  decode (Right _) = Nothing
  encodeOp = Right
  decodeOp (Left _) = Nothing
  decodeOp (Right x) = Just x

deriving instance Show (Operation ())
instance Arbitrary (Operation ()) where
  arbitrary = pure UnitId

-- instance Show (Operation ()) where
--   show
