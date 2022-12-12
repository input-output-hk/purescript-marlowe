module Spec.TypeId (TypeId(..)) where

import Prelude

import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , decodeJson
  , encodeJson
  )

newtype TypeId = TypeId String

derive newtype instance Show TypeId
instance DecodeJson TypeId where
  decodeJson j = TypeId <$> decodeJson j

instance EncodeJson TypeId where
  encodeJson (TypeId t) = encodeJson t