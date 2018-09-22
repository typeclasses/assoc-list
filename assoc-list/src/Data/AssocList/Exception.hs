module Data.AssocList.Exception where

-- base
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Prelude (Eq, Show)

-- | This exception shows up when one attempts to retrieve a value by key
-- from an associative list using a partial function with a type signature
-- such as
--
-- > (!) :: Eq a => AssocList a b -> a -> b
--
-- but no result can be obtained because the requested key is not present
-- exists in the mapping.

data MissingAssocListKey = MissingAssocListKey
  deriving (Eq, Show, Typeable)

instance Exception MissingAssocListKey
