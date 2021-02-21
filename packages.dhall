
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20210118/packages.dhall sha256:a59c5c93a68d5d066f3815a89f398bcf00e130a51cb185b2da29b20e2d8ae115

in  upstream
  with prismatic =
      { dependencies =
          [ "aff"
          , "arrays"
          , "console"
          , "effect"
          , "freet"
          , "prelude"
          , "profunctor-lenses"
          , "psci-support"
          , "react"
          , "react-dom"
          , "refs"
          , "web-html"
          ]
      , repo =
          "https://github.com/jac3km4/purescript-prismatic.git"
      , version =
          "update"
      }
