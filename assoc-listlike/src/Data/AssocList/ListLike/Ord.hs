{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ViewPatterns #-}

-- | Functions on association lists that make use of an 'Ord' constraint
-- on the type of the keys.

module Data.AssocList.ListLike.Ord
    (

    -- * Related modules
    -- $relatedModules

    -- * Sorting
      sortKeys

    ) where

import Data.AssocList.ListLike.Concept

-- base
import Data.Ord (comparing)
import Prelude (Ord (..))

-- ListLike
import Data.ListLike (ListLike, cons, uncons)
import qualified Data.ListLike as LL

-- $relatedModules
-- A module that is a lot like this one:
--
-- * "Data.AssocList.ListLike.Comparison" - Functions on association
--   lists that make use of a 'Comparison' on the keys

-- | Sort an association list according by its keys. This is a stable
-- sort, so when a key appears multiple times in the input list, the
-- ordering of its values in the resulting list remains unchanged.
--
-- >>> sortKeys [(2, 'b'), (3, 'c'), (2, 'a'), (4, 'd'), (2, 'e'), (1, 'f')]
-- [(1,'f'),(2,'b'),(2,'a'),(2,'e'),(3,'c'),(4,'d')]

sortKeys :: forall l a b. (ListLike l (a, b), Ord a)
    => l -> l
sortKeys = LL.sortBy (comparing (\(a, b) -> a))
