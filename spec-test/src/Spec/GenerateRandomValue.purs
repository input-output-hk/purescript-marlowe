module Spec.GenerateRandomValue where

import Prelude

import Control.Monad.Gen (elements)
import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Effect (Effect)
import Language.Marlowe.Core.Semantics.Gen
  ( genAction
  , genBound
  , genCase
  , genChoiceId
  , genContract
  , genInput
  , genObservation
  , genParty
  , genPayee
  , genToken
  , genValue
  )
import Spec.TypeId (TypeId(..))
import Test.QuickCheck.Gen (Gen, randomSampleOne)

data RandomResponse a
  = RandomValue a
  | UnknownType TypeId

instance EncodeJson a => EncodeJson (RandomResponse a) where
  encodeJson (RandomValue value) =
    encodeJson { value }
  encodeJson (UnknownType t) =
    encodeJson { "unknown-type": t }

generateRandomValue :: Int -> TypeId -> Effect (RandomResponse Json)
generateRandomValue size t = randomSampleOne $ case t of
  TypeId "Core.Action" -> value $ genAction size
  TypeId "Core.Bound" -> value $ genBound
  TypeId "Core.Case" -> value $ genCase size
  TypeId "Core.ChoiceId" -> value $ genChoiceId
  TypeId "Core.Contract" -> value $ genContract
  TypeId "Core.Token" -> value $ genToken
  TypeId "Core.Payee" -> value $ genPayee
  TypeId "Core.Input" -> value $ genInput
  TypeId "Core.Observation" -> value $ genObservation
  TypeId "Core.Value" -> value $ genValue
  TypeId "Core.Party" -> value $ genParty
  _ -> elements (NonEmptyArray ([ UnknownType t ]))
  where
  value :: forall a. EncodeJson a => Gen a -> Gen (RandomResponse Json)
  value g = RandomValue <<< encodeJson <$> g

