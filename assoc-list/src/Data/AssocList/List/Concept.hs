module Data.AssocList.List.Concept
    (

    -- * Association list type
      AssocList

    -- * Related modules
    -- $relatedModules

    -- * Exception
    , MissingAssocListKey (..)

    ) where

-- base
import Control.Exception (Exception)
import Data.Typeable     (Typeable)
import Prelude           (Eq, Show)

-- $relatedModules
-- * "Data.AssocList.List.Eq" - Functions that involve @Eq@ constraints
--   on the keys
-- * "Data.AssocList.List.Equivalence" - Most of the same functions as
--   the @Eq@ module, but with an @Equivalence@ parameter instead of an
--   @Eq@ constraint
-- * "Data.AssocList.List.Predicate" - Most of the same functions as the
--   @Eq@ module, but specifying keys using a @Predicate@ rather than a
--   particular key
-- * "Data.AssocList.List.Ord" - Functions that involve @Ord@ constraints
--   on the keys
-- * "Data.AssocList.List.Comparison" - The same functions as the @Ord@
--   module, but with a @Comparison@ parameter instead of an @Ord@
--   constraint.

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
