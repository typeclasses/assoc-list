module Data.AssocList.List.Eq where

import Data.AssocList.Exception
import Data.AssocList.List.Type

-- base
import Control.Exception (throw)
import Prelude (Eq (..), Maybe (..), error, otherwise)

(!) :: Eq a => AssocList a b -> a -> b
[] ! key                           = throw MissingAssocListKey
((x, y) : xys) ! key
        | key == x                 = y
        | otherwise                = xys ! key

(!?) :: Eq a => AssocList a b -> a -> Maybe b
[] !? key                          = Nothing
((x, y) : xys) !? key
        | key == x                 = Just y
        | otherwise                = xys !? key
