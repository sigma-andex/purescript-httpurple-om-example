{ name = "httpurple-om-example"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "httpurple"
  , "maybe"
  , "prelude"
  , "record"
  , "record-studio"
  , "transformers"
  , "typelevel-prelude"
  , "yoga-om"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT-0"
}
