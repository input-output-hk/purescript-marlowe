module Test.SpecMain where

import Prelude

import Data.Argonaut (Json, decodeJson, encodeJson, stringify)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Language.Marlowe.Core.V1.Semantics (computeTransaction) as C
import Node.Process (stdin)
import Test.JsonStream (createJsonStream)
import Test.Request (Request(..))
import Test.Response (Response(..))
import Test.RoundtripSerialization (testRoundtripSerializationJson)

handleJsonRequest :: Json -> Response Json
handleJsonRequest req = case decodeJson req of
  Left _ -> UnknownRequest
  Right (TestRoundtripSerialization typeId json) ->
    RequestResponse
      $ encodeJson
      $ testRoundtripSerializationJson typeId json
  Right (GenerateRandomValue _) -> RequestNotImplemented
  Right (ComputeTransaction input state contract) ->
    RequestResponse
      $ encodeJson
      $ C.computeTransaction input state contract

invalidJsonRequest :: String -> Response Json
invalidJsonRequest = InvalidRequest

main :: Effect Unit
main = createJsonStream
  { stream: stdin
  , slizeSize: 4096
  , beginSeparator: "```"
  , endSeparator: "```"
  , onJson: \req -> log $ stringify $ encodeJson $ handleJsonRequest req
  , onError: \err -> log $ stringify $ encodeJson $ invalidJsonRequest $ show
      err
  , onFinish: log "Finished!"
  }