{ name = "marlowe"
, license = "MIT"
, repository = "https://github.com/input-output-hk/purescript-marlowe.git"
, dependencies =
  [ "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "bigints"
  , "contravariant"
  , "control"
  , "datetime"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "foreign-object"
  , "functions"
  , "gen"
  , "identity"
  , "integers"
  , "json-helpers"
  , "lists"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
