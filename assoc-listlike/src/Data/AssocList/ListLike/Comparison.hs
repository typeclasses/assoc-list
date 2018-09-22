{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ViewPatterns #-}

-- | Functions on association lists that make use of a 'Comparison'
-- on the keys.

module Data.AssocList.ListLike.Comparison
    (

    -- * Related modules
    -- $relatedModules

    -- * Sorting
      sortKeys

    ) where

import Data.AssocList.ListLike.Concept

-- base
import Prelude ()

-- contravariant
import Data.Functor.Contravariant (Comparison (..), contramap)

-- ListLike
import Data.ListLike (cons, uncons)
import qualified Data.ListLike as LL

-- $setup
-- >>> import Data.Functor.Contravariant (defaultComparison)

-- $relatedModules
-- A module that is a lot like this one:
--
-- * "Data.AssocList.ListLike.Ord" - Functions on association lists
--   that make use of an 'Ord' constraint on the type of the keys

-- | Sort an association list according to a particular 'Comparison'
-- of its keys. This is a stable sort, so when a key appears multiple
-- times in the input list, the ordering of its values in the
-- resulting list remains unchanged.
--
-- >>> sortKeys defaultComparison [(2, 'b'), (3, 'c'), (2, 'a'), (4, 'd'), (2, 'e'), (1, 'f')]
-- [(1,'f'),(2,'b'),(2,'a'),(2,'e'),(3,'c'),(4,'d')]

sortKeys :: forall l a b. AssocList l a b
    => Comparison a -> l -> l
sortKeys cmp =
  let
    cmp' = contramap (\(a, b) -> a) cmp
  in
    LL.sortBy (getComparison cmp')
