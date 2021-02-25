let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0/packages.dhall sha256:710b53c085a18aa1263474659daa0ae15b7a4f453158c4f60ab448a6b3ed494e

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
  with simple-json =
    { dependencies =
      [ "prelude"
      , "typelevel-prelude"
      , "record"
      , "variant"
      , "nullable"
      , "foreign-object"
      , "foreign"
      , "exceptions"
      , "arrays"
      ]
    , repo = "https://github.com/srghma/purescript-simple-json.git"
    , version = "master"
    }
  with variant =
    { dependencies =
      [ "prelude"
      , "tuples"
      , "unsafe-coerce"
      , "partial"
      , "maybe"
      , "lists"
      , "record"
      , "enums"
      ]
    , repo = "https://github.com/JordanMartinez/purescript-variant.git"
    , version = "updateTo0.14"
    }
