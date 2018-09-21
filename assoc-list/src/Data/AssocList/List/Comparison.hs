module Data.AssocList.List.Comparison where

import Data.AssocList.List.Type

-- base
import qualified Data.List as List

-- contravariant
import Data.Functor.Contravariant (Comparison (..), contramap)

sortKeys :: Comparison a -> AssocList a b -> AssocList a b
sortKeys cmp =
  let
    cmp' = contramap (\(a, b) -> a) cmp
  in
    List.sortBy (getComparison cmp')
