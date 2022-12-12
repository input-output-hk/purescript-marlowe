module Test.Control.JsonStream
  ( tests
  ) where

import Prelude

import Control.Coroutine (Producer, await, pullFrom, runProcess)
import Control.Coroutine.Aff (emit, produce)
import Control.JsonStream (JsonStreamError(..), createJsonStream)
import Data.Argonaut (Json, fromNumber, jsonEmptyObject, stringify)
import Data.Either (Either)
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.Stream (Duplex, Readable, end, writeString)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

foreign import streamFromString :: String -> Effect (Readable ())
foreign import duplexStream :: Effect Duplex

type Cb = Either Error Unit -> Effect Unit

data Ev
  = OnJson Json
  | OnError JsonStreamError
  | OnFinish

instance Show Ev where
  show (OnJson json) = "(OnJson " <> stringify json <> ")"
  show (OnError err) = "(OnError " <> show err <> ")"
  show OnFinish = "OnFinish"

derive instance Eq Ev

jsonStreamProducer'
  :: forall s
   . { beginSeparator :: String
     , endSeparator :: String
     , sliceSize :: Int
     }
  -> Readable s
  -> Producer Ev Aff Unit
jsonStreamProducer' opts stream = produce \emitter ->
  createJsonStream
    { stream
    , sliceSize: opts.sliceSize
    , beginSeparator: opts.beginSeparator
    , endSeparator: opts.endSeparator
    , onJson: emit emitter <<< OnJson
    , onError: emit emitter <<< OnError
    , onFinish: emit emitter OnFinish
    }

jsonStreamProducer :: forall s. Readable s -> Producer Ev Aff Unit
jsonStreamProducer = jsonStreamProducer'
  { sliceSize: 1000
  , beginSeparator: "```"
  , endSeparator: "```"
  }

tests :: Spec Unit
tests = describe "JsonStream" do
  describe "createJsonStream" do
    it "should finish after the stream is done" do
      stream <- liftEffect $ streamFromString ""
      let
        test = do
          ev <- await
          ev `shouldEqual` OnFinish

      runProcess (test `pullFrom` jsonStreamProducer stream)

    it "should call onJson on a valid json" do
      stream <- liftEffect $ streamFromString "```{}```"
      let
        test = do
          ev1 <- await
          ev1 `shouldEqual` OnJson jsonEmptyObject
          ev2 <- await
          ev2 `shouldEqual` OnFinish

      runProcess (test `pullFrom` jsonStreamProducer stream)

    it "should call onJson on a valid json with newlines" do
      stream <- liftEffect $ streamFromString "```\n{\n}\n```\n"
      let
        test = do
          ev1 <- await
          ev1 `shouldEqual` OnJson jsonEmptyObject
          ev2 <- await
          ev2 `shouldEqual` OnFinish

      runProcess (test `pullFrom` jsonStreamProducer stream)

    it "should discard characters before the begin separator" do
      stream <- liftEffect $ streamFromString "abcde```{}```"
      let
        test = do
          ev1 <- await
          ev1 `shouldEqual` OnJson jsonEmptyObject
          ev2 <- await
          ev2 `shouldEqual` OnFinish

      runProcess (test `pullFrom` jsonStreamProducer stream)
    it "should discard characters after the end separator" do
      stream <- liftEffect $ streamFromString "```{}```abcde"
      let
        test = do
          ev1 <- await
          ev1 `shouldEqual` OnJson jsonEmptyObject
          ev2 <- await
          ev2 `shouldEqual` OnFinish

      runProcess (test `pullFrom` jsonStreamProducer stream)

    it "should throw onError on invalid json" do
      stream <- liftEffect $ streamFromString "```abc```"
      let
        test = do
          ev1 <- await
          ev1 `shouldEqual` OnError InvalidJson
          ev2 <- await
          ev2 `shouldEqual` OnFinish

      runProcess (test `pullFrom` jsonStreamProducer stream)

    it "should recover after error" do
      stream <- liftEffect $ streamFromString "```abc``` ```9```"
      let
        test = do
          ev1 <- await
          ev1 `shouldEqual` OnError InvalidJson
          ev2 <- await
          ev2 `shouldEqual` OnJson (fromNumber 9.0)
          ev3 <- await
          ev3 `shouldEqual` OnFinish

      runProcess (test `pullFrom` jsonStreamProducer stream)

    it "should discard interrupted entries" do
      stream <- liftEffect $ streamFromString "```9"
      let
        test = do
          ev1 <- await
          ev1 `shouldEqual` OnFinish

      runProcess (test `pullFrom` jsonStreamProducer stream)

    -- Even if this looks like the previous one, it is catching a bug
    -- that the previous test didn't catch.
    it "should not fail when just receiving the begin separator" do
      stream <- liftEffect $ streamFromString "```"
      let
        test = do
          ev1 <- await
          ev1 `shouldEqual` OnFinish

      runProcess (test `pullFrom` jsonStreamProducer stream)

    it "should increase the buffer size when needed" do
      stream <- liftEffect $ streamFromString "```     {}     ```"
      let
        opts =
          { sliceSize: 5
          , beginSeparator: "```"
          , endSeparator: "```"
          }

        test = do
          ev1 <- await
          ev1 `shouldEqual` OnJson jsonEmptyObject
          ev2 <- await
          ev2 `shouldEqual` OnFinish

      runProcess (test `pullFrom` jsonStreamProducer' opts stream)

    it "should work with data in multiple chunks" do
      stream <- liftEffect $ duplexStream
      let
        writeString' str = void $ writeString stream UTF8 str
          (const $ pure unit)
      liftEffect do
        writeString' "```"
        writeString' "8"
        writeString' "```"
        end stream (const $ pure unit)
      let
        test = do
          ev1 <- await
          ev1 `shouldEqual` OnJson (fromNumber 8.0)
          ev2 <- await
          ev2 `shouldEqual` OnFinish

      runProcess (test `pullFrom` jsonStreamProducer stream)

