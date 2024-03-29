let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220816/packages.dhall
        sha256:8b4467b4b5041914f9b765779c8936d6d4c230b1f60eb64f6269c71812fd7e98

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
    , repo = "https://github.com/jac3km4/purescript-prismatic.git"
    , version = "update"
    }

