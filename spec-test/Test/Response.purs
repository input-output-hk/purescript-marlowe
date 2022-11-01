module Test.Response (Response(..)) where

import Data.Argonaut (class EncodeJson, Json, encodeJson)

data Response transport
  = InvalidRequest String
  | UnknownRequest
  | RequestResponse transport
  | RequestNotImplemented

instance EncodeJson (Response Json) where
  encodeJson UnknownRequest =
    encodeJson "UnknownRequest"
  encodeJson RequestNotImplemented =
    encodeJson "RequestNotImplemented"
  encodeJson (RequestResponse res) =
    encodeJson { "request-response": res }
  encodeJson (InvalidRequest err) =
    encodeJson { "invalid-request": err }
