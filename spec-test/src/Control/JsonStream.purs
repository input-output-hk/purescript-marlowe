module Control.JsonStream
  ( createJsonStream
  , JsonStreamError(..)
  , CreateJsonStreamOptions
  ) where

import Prelude

import Data.Argonaut (Json)
import Effect (Effect)
import Node.Stream (Readable)
import Record.Builder (build, insert)
import Type.Proxy (Proxy(..))

data JsonStreamError
  = StreamError
  | InvalidJson

derive instance Eq JsonStreamError

instance Show JsonStreamError where
  show StreamError = "Stream Error"
  show InvalidJson = "Invalid Json"

type FFIOptions s =
  ( streamError :: JsonStreamError
  , invalidJson :: JsonStreamError
  | CreateJsonStreamOptions s
  )

-- This Function starts reading a stream, discarding characters until
-- it reaches the begin separator, then it reads until it reaches the
-- end separator. If the inner string can be parsed as Json, it fires
-- an onJson event.
foreign import _createJsonStream
  :: forall s
   . { | FFIOptions s }
  -> Effect Unit

type CreateJsonStreamOptions s =
  ( stream :: Readable s
  , slizeSize :: Int
  , beginSeparator :: String
  , endSeparator :: String
  , onJson :: Json -> Effect Unit
  , onError :: JsonStreamError -> Effect Unit
  , onFinish :: Effect Unit
  )

createJsonStream
  :: forall s
   . { | CreateJsonStreamOptions s }
  -> Effect Unit
createJsonStream provided = _createJsonStream fullOptions
  where
  fullOptions =
    build
      ( insert (Proxy :: Proxy "streamError") StreamError
          <<< insert (Proxy :: Proxy "invalidJson") InvalidJson
      )
      provided

