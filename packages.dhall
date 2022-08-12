let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.2-20220706/packages.dhall sha256:7a24ebdbacb2bfa27b2fc6ce3da96f048093d64e54369965a2a7b5d9892b6031

let overrides = {=}

let additions =
      { json-helpers =
        { dependencies =
          [ "aff"
          , "argonaut-codecs"
          , "argonaut-core"
          , "arrays"
          , "bifunctors"
          , "contravariant"
          , "control"
          , "effect"
          , "either"
          , "enums"
          , "foldable-traversable"
          , "foreign-object"
          , "maybe"
          , "newtype"
          , "ordered-collections"
          , "prelude"
          , "profunctor"
          , "psci-support"
          , "quickcheck"
          , "record"
          , "spec"
          , "spec-quickcheck"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          ]
        , repo =
            "https://github.com/input-output-hk/purescript-bridge-json-helpers.git"
        , version = "60615c36abaee16d8dbe09cdd0e772e6d523d024"
        }
      }

in  upstream ⫽ overrides ⫽ additions
