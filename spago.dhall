{ name = "cyberdoc"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "integers"
  , "lists"
  , "maybe"
  , "milkis"
  , "prelude"
  , "prismatic"
  , "react"
  , "simple-json"
  , "strings"
  , "transformers"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
