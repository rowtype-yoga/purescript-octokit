{ name = "purescript-octokit"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut"
  , "bifunctors"
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
, license = "MIT-0"
, repository = "https://github.com/sigma-andex/purescript-octokit.git"
}
