let conf = ../spago.dhall

in    conf
    ⫽ { dependencies =
          conf.dependencies # [ "console", "node-process", "node-streams" ]
      , sources = conf.sources # [ "spec-test/**/*.purs" ]
      }
