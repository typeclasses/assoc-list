{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Data.AssocList.ListLike.Concept
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

-- ListLike
import Data.ListLike (ListLike)

-- $relatedModules
-- * "Data.AssocList.ListLike.Eq" - Functions that involve @Eq@
--   constraints on the keys
-- * "Data.AssocList.ListLike.Equivalence" - Most of the same functions
--   as the @Eq@ module, but with an @Equivalence@ parameter instead of
--   an @Eq@ constraint
-- * "Data.AssocList.ListLike.Predicate" - Most of the same functions as
--   the @Eq@ module, but specifying keys using a @Predicate@ rather
--   than a particular key
-- * "Data.AssocList.ListLike.Ord" - Functions that involve @Ord@
--   constraints on the keys
-- * "Data.AssocList.ListLike.Comparison" - The same functions as the
--   @Ord@ module, but with a @Comparison@ parameter instead of an @Ord@
--   constraint.

type AssocList l a b = ListLike l (a, b)

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
