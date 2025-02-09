{ name = "cardano-provider"
, dependencies =
  [ "aeson"
  , "aff"
  , "aff-promise"
  , "affjax"
  , "argonaut-codecs"
  , "cardano-types"
  , "console"
  , "effect"
  , "either"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
