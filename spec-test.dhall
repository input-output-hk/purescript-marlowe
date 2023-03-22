let marloweConf = ./spago.dhall

in  { name = "marlowe-spec-cli"
    , license = "MIT"
    , repository = "https://github.com/input-output-hk/purescript-marlowe.git"
    , dependencies =
          marloweConf.dependencies
        # [ "console"
          , "effect"
          , "node-process"
          , "node-streams"
          , "node-buffer"
          , "gen"
          , "nonempty"
          , "quickcheck"
          , "record"
          , "tailrec"
          , "aff"
          , "coroutines"
          , "aff-coroutines"
          , "spec"
          , "lcg"
          ]
    , packages = ./packages.dhall
    , sources = [ "src/**/*.purs", "spec-test/**/*.purs" ]
    }
