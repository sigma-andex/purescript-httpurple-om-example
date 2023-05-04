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
  , "yoga-om"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT-0"
}
