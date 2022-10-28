module Test.Request where

import Prelude

import Data.Argonaut (class DecodeJson, Json)
import Data.Argonaut.Extra (getProp, object, requireProp)
import Data.Maybe (Maybe(..))
import Language.Marlowe.Core.V1.Semantics.Types
  ( Contract
  , State
  , TransactionInput
  ) as C
import Test.RoundtripSerialization (TypeId)

data Request transport
  = TestRoundtripSerialization TypeId transport
  | GenerateRandomValue TypeId
  | ComputeTransaction C.TransactionInput C.State C.Contract

instance DecodeJson (Request Json) where
  decodeJson =
    object "Request" do
      requestType <- requireProp "request"
      mTypeId <- getProp "typeId"
      mJson <- getProp "json"
      mTransactionInput <- getProp "transactionInput"
      mState <- getProp "state"
      mContract <- getProp "coreContract"
      case requestType of
        "test-roundtrip-serialization" -> pure $
          (TestRoundtripSerialization <$> mTypeId <*> mJson)
        "generate-random-value" -> pure $ (GenerateRandomValue <$> mTypeId)
        "compute-transaction" -> pure $
          (ComputeTransaction <$> mTransactionInput <*> mState <*> mContract)
        _ -> pure Nothing

