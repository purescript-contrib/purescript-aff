{ name = "aff"
, dependencies =
  [ "assert"
  , "console"
  , "datetime"
  , "effect"
  , "exceptions"
  , "free"
  , "functions"
  , "minibench"
  , "parallel"
  , "partial"
  , "psci-support"
  , "transformers"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
