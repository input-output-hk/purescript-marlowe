module Spec.Main where

import Prelude

import Control.JsonStream (createJsonStream)
import Data.Argonaut (Json, decodeJson, encodeJson, stringify)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe, maybe)
import Effect (Effect)
import Effect.Console (log)
import Language.Marlowe.Core.V1.Semantics (computeTransaction, playTrace) as C
import Node.Process (stdin)
import Random.LCG (randomSeed)
import Spec.GenerateRandomValue (generateRandomValue)
import Spec.Request (Request(..))
import Spec.Response (Response(..))
import Spec.RoundtripSerialization (testRoundtripSerializationJson)

handleJsonRequest :: Json -> Effect (Response Json)
handleJsonRequest req = case decodeJson req of
  Left _ -> pure UnknownRequest
  Right (TestRoundtripSerialization typeId json) ->
    pure
      $ RequestResponse
      $ encodeJson
      $ testRoundtripSerializationJson typeId json
  Right (GenerateRandomValue typeId mSize mSeed) -> do
    let
      size = fromMaybe 5 mSize
    seed <- maybe randomSeed pure mSeed
    pure $ RequestResponse
      $ encodeJson
      $ generateRandomValue seed size typeId
  Right (ComputeTransaction input state contract) ->
    pure
      $ RequestResponse
      $ encodeJson
      $ C.computeTransaction input state contract
  Right (PlayTrace initialTime contract inputs) ->
    pure
      $ RequestResponse
      $ encodeJson
      $ C.playTrace initialTime contract inputs

invalidJsonRequest :: String -> Response Json
invalidJsonRequest = InvalidRequest

main :: Effect Unit
main = createJsonStream
  { stream: stdin
  , sliceSize: 4096
  , beginSeparator: "```"
  , endSeparator: "```"
  , onJson: \req -> log <<< stringify <<< encodeJson =<< handleJsonRequest req
  , onError: \err -> log $ stringify $ encodeJson $ invalidJsonRequest $ show
      err
  , onFinish: log "Finished!"
  }