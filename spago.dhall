{ name = "purescript-octokit"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut"
  , "console"
  , "effect"
  , "either"
  , "functions"
  , "maybe"
  , "newtype"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
