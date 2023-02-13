{ name = "bytestrings"
, dependencies =
    [ "arrays"
    , "console"
    , "effect"
    , "exceptions"
    , "foldable-traversable"
    , "integers"
    , "leibniz"
    , "maybe"
    , "newtype"
    , "node-buffer"
    , "partial"
    , "prelude"
    , "quickcheck"
    , "quickcheck-laws"
    , "quotient"
    , "unsafe-coerce"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
