-- | Functions on 'AssocList's that make use of a 'Comparison'
-- on the keys.

module Data.AssocList.List.Comparison
    (

    -- * Related modules
    -- $relatedModules

    -- * Sorting
      sortKeys

    ) where

import Data.AssocList.List.Type

-- base
import qualified Data.List

-- contravariant
import Data.Functor.Contravariant (Comparison (..), contramap)

-- $relatedModules
-- A module that is a lot like this one:
--
-- * "Data.AssocList.List.Ord" - Functions on 'AssocList's that
--   make use of an 'Ord' constraint on the type of the keys

sortKeys :: Comparison a -> AssocList a b -> AssocList a b
sortKeys cmp =
  let
    cmp' = contramap (\(a, b) -> a) cmp
  in
    Data.List.sortBy (getComparison cmp')
