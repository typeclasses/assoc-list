module Data.AssocList.List.Ord where

import Data.AssocList.List.Type

-- base
import qualified Data.List as List
import Prelude (Ord (..), Maybe (..))

sortKeys :: Ord a => AssocList a b -> AssocList a b
sortKeys = List.sortOn (\(a, b) -> a)
