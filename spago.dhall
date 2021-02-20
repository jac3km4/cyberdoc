{ name = "cyberdoc"
, dependencies =
  [ "effect"
  , "prismatic"
  , "simple-json"
  , "milkis"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
