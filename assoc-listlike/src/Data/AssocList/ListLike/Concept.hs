module Data.AssocList.ListLike.Concept
    ( MissingAssocListKey (..)
    ) where

-- base
import Control.Exception (Exception)
import Data.Typeable     (Typeable)
import Prelude           (Eq, Show)

-- | This exception shows up when one attempts to retrieve a value by key
-- from an association list using a partial function with a type signature
-- such as
--
-- > (!) :: (LikeLike l (a, b), Eq a) => l -> a -> b
--
-- but no result can be obtained because the requested key is not present
-- exists in the mapping.

data MissingAssocListKey = MissingAssocListKey
  deriving (Eq, Show, Typeable)

instance Exception MissingAssocListKey
