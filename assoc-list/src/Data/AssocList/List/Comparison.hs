module Data.AssocList.List.Comparison
    ( sortKeys
    ) where

import Data.AssocList.List.Type

-- base
import qualified Data.List

-- contravariant
import Data.Functor.Contravariant (Comparison (..), contramap)

sortKeys :: Comparison a -> AssocList a b -> AssocList a b
sortKeys cmp =
  let
    cmp' = contramap (\(a, b) -> a) cmp
  in
    Data.List.sortBy (getComparison cmp')
