module Spec.RoundtripSerialization where

import Prelude

import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , Json
  , decodeJson
  , encodeJson
  , printJsonDecodeError
  )
import Data.Either (Either(..))
import Language.Marlowe.Core.V1.Semantics.Types
  ( Action
  , Bound
  , Case
  , ChoiceId
  , Contract
  , Input
  , Observation
  , Party
  , Payee
  , Payment
  , State
  , Token
  , TransactionError
  , TransactionInput
  , TransactionOutput
  , TransactionWarning
  , Value
  ) as C
import Language.Marlowe.Extended.V1 as EM
import Spec.TypeId (TypeId(..))
import Type.Proxy (Proxy(..))

data SerializationResponse transport
  = SerializationSuccess transport
  | UnknownType TypeId
  | SerializationError String

instance EncodeJson (SerializationResponse Json) where
  encodeJson (SerializationSuccess result) =
    encodeJson { "serialization-success": result }
  encodeJson (UnknownType t) =
    encodeJson { "unknown-type": t }
  encodeJson (SerializationError err) =
    encodeJson { "serialization-error": err }

testRoundtripSerializationJson :: TypeId -> Json -> SerializationResponse Json
testRoundtripSerializationJson typeId = case typeId of
  TypeId "Core.Action" -> roundTrip (Proxy :: Proxy C.Action)
  TypeId "Core.Bound" -> roundTrip (Proxy :: Proxy C.Bound)
  TypeId "Core.Case" -> roundTrip (Proxy :: Proxy C.Case)
  TypeId "Core.ChoiceId" -> roundTrip (Proxy :: Proxy C.ChoiceId)
  TypeId "Core.Contract" -> roundTrip (Proxy :: Proxy C.Contract)
  TypeId "Core.Token" -> roundTrip (Proxy :: Proxy C.Token)
  TypeId "Core.Payee" -> roundTrip (Proxy :: Proxy C.Payee)
  TypeId "Core.Input" -> roundTrip (Proxy :: Proxy C.Input)
  TypeId "Core.Observation" -> roundTrip (Proxy :: Proxy C.Observation)
  TypeId "Core.Value" -> roundTrip (Proxy :: Proxy C.Value)
  TypeId "Core.Payment" -> roundTrip (Proxy :: Proxy C.Payment)
  TypeId "Core.Party" -> roundTrip (Proxy :: Proxy C.Party)
  TypeId "Core.State" -> roundTrip (Proxy :: Proxy C.State)
  TypeId "Core.TransactionError" -> roundTrip
    (Proxy :: Proxy C.TransactionError)
  TypeId "Core.TransactionOutput" -> roundTrip
    (Proxy :: Proxy C.TransactionOutput)
  TypeId "Core.TransactionWarning" -> roundTrip
    (Proxy :: Proxy C.TransactionWarning)
  TypeId "Core.Transaction" -> roundTrip (Proxy :: Proxy C.TransactionInput)
  TypeId "Extended.Timeout" -> roundTrip (Proxy :: Proxy EM.Timeout)
  TypeId "Extended.Value" -> roundTrip (Proxy :: Proxy EM.Value)
  TypeId "Extended.Observation" -> roundTrip (Proxy :: Proxy EM.Observation)
  TypeId "Extended.Action" -> roundTrip (Proxy :: Proxy EM.Action)
  TypeId "Extended.Payee" -> roundTrip (Proxy :: Proxy EM.Payee)
  TypeId "Extended.Case" -> roundTrip (Proxy :: Proxy EM.Case)
  TypeId "Extended.Contract" -> roundTrip (Proxy :: Proxy EM.Contract)
  TypeId "Extended.Module" -> roundTrip (Proxy :: Proxy EM.Module)

  _ -> const $ UnknownType typeId
  where
  roundTrip
    :: forall a
     . DecodeJson a
    => EncodeJson a
    => Proxy a
    -> Json
    -> SerializationResponse Json
  roundTrip _ json = case decodeJson json of
    Right (x :: a) -> SerializationSuccess $ encodeJson x
    Left err -> SerializationError $ printJsonDecodeError err