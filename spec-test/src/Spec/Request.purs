module Spec.Request where

import Prelude

import Control.Bind (bindFlipped)
import Data.Argonaut (class DecodeJson, Json)
import Data.Argonaut.Extra (getProp, object, requireProp)
import Data.DateTime.Instant (instant)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Language.Marlowe.Core.V1.Semantics.Types
  ( Contract
  , Environment
  , Observation
  , State
  , Timeout
  , TransactionInput
  , Value
  ) as C
import Random.LCG (Seed, mkSeed)
import Spec.TypeId (TypeId)
import Test.QuickCheck.Gen (Size)

data Request transport
  = TestRoundtripSerialization TypeId transport
  | GenerateRandomValue TypeId (Maybe Size) (Maybe Seed)
  | ComputeTransaction C.TransactionInput C.State C.Contract
  | PlayTrace C.Timeout C.Contract (List C.TransactionInput)
  | EvalValue C.Environment C.State C.Value
  | EvalObservation C.Environment C.State C.Observation

instance DecodeJson (Request Json) where
  decodeJson =
    object "Request" do
      requestType <- requireProp "request"
      mTypeId <- getProp "typeId"
      mJson <- getProp "json"
      mTransactionInput <- getProp "transactionInput"
      mTransactionInputs <- getProp "transactionInputs"
      mState <- getProp "state"
      mContract <- getProp "coreContract"
      mValue <- getProp "value"
      mObservation <- getProp "observation"
      mEnvironment <- getProp "environment"
      mInitialTime <- bindFlipped (instant <<< Milliseconds) <$> getProp
        "initialTime"
      mSize <- getProp "size"
      mSeed <- bindFlipped (Just <<< mkSeed) <$> getProp "seed"
      case requestType of
        "test-roundtrip-serialization" -> pure
          (TestRoundtripSerialization <$> mTypeId <*> mJson)
        "generate-random-value" -> pure
          (GenerateRandomValue <$> mTypeId <*> pure mSize <*> pure mSeed)
        "compute-transaction" -> pure
          (ComputeTransaction <$> mTransactionInput <*> mState <*> mContract)
        "playtrace" -> pure
          (PlayTrace <$> mInitialTime <*> mContract <*> mTransactionInputs)
        "eval-value" -> pure
          (EvalValue <$> mEnvironment <*> mState <*> mValue)
        "eval-observation" -> pure
          (EvalObservation <$> mEnvironment <*> mState <*> mObservation)
        _ -> pure Nothing

