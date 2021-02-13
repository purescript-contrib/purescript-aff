let config = ./spago.dhall
in config
    with dependencies = (config.dependencies # [ "random" ])
    with sources = (config.sources # [ "examples/**/*.purs" ])
