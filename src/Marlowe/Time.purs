module Marlowe.Time where

import Prelude

import Control.Monad.Reader (ReaderT(..))
import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Argonaut.Decode.Aeson (Decoder)
import Data.Argonaut.Encode.Aeson (Encoder)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (Either, note)
import Data.Int as Int
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Data.Op (Op(..))
import Data.Time.Duration (Milliseconds(..))
import Partial.Unsafe (unsafePartial)

unixEpoch :: Instant
unixEpoch = unsafeInstantFromInt 0

unsafeInstantFromInt :: Int -> Instant
unsafeInstantFromInt ms =
  unsafePartial $ fromJust $ instant $ Milliseconds $ Int.toNumber ms

instantToJson :: Instant -> Json
instantToJson = encodeJson <<< unwrap <<< unInstant

instantDecoder :: Decoder Instant
instantDecoder = ReaderT instantFromJson

instantEncoder :: Encoder Instant
instantEncoder = Op instantToJson

instantFromJson :: Json -> Either JsonDecodeError Instant
instantFromJson json = do
  ms <- decodeJson json
  note (TypeMismatch "UNIX timestamp") $ instant $ Milliseconds ms
