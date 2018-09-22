import Test.DocTest

main :: IO ()
main =
  doctest
    [ "-isrc"
    , "src/Data/AssocList/ListLike/Comparison.hs"
    , "src/Data/AssocList/ListLike/Eq.hs"
    , "src/Data/AssocList/ListLike/Equivalence.hs"
    , "src/Data/AssocList/ListLike/Ord.hs"
    , "src/Data/AssocList/ListLike/Predicate.hs"
    ]
