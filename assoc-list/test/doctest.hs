import Test.DocTest

main :: IO ()
main =
  doctest
    [ "-isrc"
    , "src/Data/AssocList/List/Comparison.hs"
    , "src/Data/AssocList/List/Eq.hs"
    , "src/Data/AssocList/List/Equivalence.hs"
    , "src/Data/AssocList/List/Ord.hs"
    , "src/Data/AssocList/List/Predicate.hs"
    ]
