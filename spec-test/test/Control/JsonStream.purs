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
import Node.Stream (Readable)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

foreign import streamFromString :: String -> Effect (Readable ())

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

asProducer :: forall s. Readable s -> Producer Ev Aff Unit
asProducer stream = produce \emitter ->
  createJsonStream
    { stream
    , slizeSize: 1000
    , beginSeparator: "```"
    , endSeparator: "```"
    , onJson: emit emitter <<< OnJson
    , onError: emit emitter <<< OnError
    , onFinish: emit emitter OnFinish
    }

tests :: Spec Unit
tests = describe "JsonStream" do
  describe "createJsonStream" do
    it "should finish after the stream is done" do
      stream <- liftEffect $ streamFromString ""
      let
        consumer = do
          ev <- await
          ev `shouldEqual` OnFinish

      runProcess (consumer `pullFrom` asProducer stream)

    it "should call onJson on a valid json" do
      stream <- liftEffect $ streamFromString "```{}```"
      let
        consumer = do
          ev1 <- await
          ev1 `shouldEqual` OnJson jsonEmptyObject
          ev2 <- await
          ev2 `shouldEqual` OnFinish

      runProcess (consumer `pullFrom` asProducer stream)

    it "should call onJson on a valid json with newlines" do
      stream <- liftEffect $ streamFromString "```\n{\n}\n```\n"
      let
        consumer = do
          ev1 <- await
          ev1 `shouldEqual` OnJson jsonEmptyObject
          ev2 <- await
          ev2 `shouldEqual` OnFinish

      runProcess (consumer `pullFrom` asProducer stream)

    it "should discard characters before the begin separator" do
      stream <- liftEffect $ streamFromString "abcde```{}```"
      let
        consumer = do
          ev1 <- await
          ev1 `shouldEqual` OnJson jsonEmptyObject
          ev2 <- await
          ev2 `shouldEqual` OnFinish

      runProcess (consumer `pullFrom` asProducer stream)
    it "should discard characters after the end separator" do
      stream <- liftEffect $ streamFromString "```{}```abcde"
      let
        consumer = do
          ev1 <- await
          ev1 `shouldEqual` OnJson jsonEmptyObject
          ev2 <- await
          ev2 `shouldEqual` OnFinish

      runProcess (consumer `pullFrom` asProducer stream)

    it "should throw onError on invalid json" do
      stream <- liftEffect $ streamFromString "```abc```"
      let
        consumer = do
          ev1 <- await
          ev1 `shouldEqual` OnError InvalidJson
          ev2 <- await
          ev2 `shouldEqual` OnFinish

      runProcess (consumer `pullFrom` asProducer stream)

    it "should recover after error" do
      stream <- liftEffect $ streamFromString "```abc``` ```9```"
      let
        consumer = do
          ev1 <- await
          ev1 `shouldEqual` OnError InvalidJson
          ev2 <- await
          ev2 `shouldEqual` OnJson (fromNumber 9.0)
          ev3 <- await
          ev3 `shouldEqual` OnFinish

      runProcess (consumer `pullFrom` asProducer stream)

    it "should discard interrupted entries" do
      stream <- liftEffect $ streamFromString "```9"
      let
        consumer = do
          ev1 <- await
          ev1 `shouldEqual` OnFinish

      runProcess (consumer `pullFrom` asProducer stream)

-- TODO: Test buffer
