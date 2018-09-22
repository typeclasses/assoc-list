module Data.AssocList.List.Concept
    ( AssocList
    , MissingAssocListKey (..)
    ) where

-- base
import Control.Exception (Exception)
import Data.Typeable     (Typeable)
import Prelude           (Eq, Show)

type AssocList a b = [(a, b)]

-- | This exception shows up when one attempts to retrieve a value by key
-- from an association list using a partial function with a type signature
-- such as
--
-- > (!) :: Eq a => AssocList a b -> a -> b
--
-- but no result can be obtained because the requested key is not present
-- exists in the mapping.

data MissingAssocListKey = MissingAssocListKey
  deriving (Eq, Show, Typeable)

instance Exception MissingAssocListKey
