let marloweConf = ./spago.dhall

in  { name = "marlowe-spec-cli"
    , license = "MIT"
    , repository = "https://github.com/input-output-hk/purescript-marlowe.git"
    , dependencies =
          marloweConf.dependencies
        # [ "console"
          , "node-process"
          , "node-streams"
          , "gen"
          , "nonempty"
          , "quickcheck"
          , "tailrec"
          ]
    , packages = ./packages.dhall
    , sources = [ "src/**/*.purs", "spec-test/**/*.purs" ]
    }
