{ name = "marlowe"
, license = "MIT"
, repository = "https://github.com/input-output-hk/purescript-marlowe.git"
, dependencies = [ "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "examples/**/*.purs" ]
}
