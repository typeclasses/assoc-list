module Data.AssocList.List.Ord
    (

    -- * Related modules
    -- $relatedModules

    -- * Sorting
      sortKeys

    -- * Re-exports
    , Ord (..)
    , Ordering (..)

    ) where

import Data.AssocList.List.Type

-- base
import qualified Data.List
import Prelude (Ord (..), Ordering (..), Maybe (..))

-- $relatedModules
-- A module that is a lot like this one:
--
-- * "Data.AssocList.List.Comparison" - Functions on 'AssocList's
--   that make use of a 'Comparison' on the keys

sortKeys :: Ord a => AssocList a b -> AssocList a b
sortKeys = Data.List.sortOn (\(a, b) -> a)
