{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ViewPatterns #-}

-- | Functions on association lists that make use of an 'Eq' constraint
-- on the type of the keys.

module Data.AssocList.ListLike.Eq
    (

    -- * Related modules
    -- $relatedModules

    -- * Lookup
      lookupFirst
    , lookupAll

    -- * Removal
    , removeFirst
    , removeAll

    -- * Mapping
    -- $mapping
    , mapFirst
    , mapAll

    -- * Alteration
    -- $alteration
    , alterFirst
    , alterAll

    -- * Grouping
    , partition
    , break
    , breakPartition

    -- * Operators
    , (!)
    , (!?)

    ) where

import Data.AssocList.ListLike.Concept

-- base
import Control.Exception (throw)
import Prelude (Eq (..), Maybe (..), maybe, error, otherwise, (<$>))

-- ListLike
import Data.ListLike (cons, uncons)
import qualified Data.ListLike as LL

-- $setup
-- >>> import Prelude (fmap, map, negate, take)

-- $relatedModules
-- Some other modules that are a lot like this one:
--
-- * "Data.AssocList.ListLike.Equivalence" - Functions on association
--   lists that involve 'Equivalence's on the keys
-- * "Data.AssocList.ListLike.Predicate" - Functions on association
--   lists that involve 'Predicate's on the keys

-- | Obtain the first value associated with a particular key.
--
-- >>> [('A',1), ('B',2), ('B',3), ('C',4)] ! 'B'
-- 2
--
-- This function is to be used only when the key must be known to
-- be present in the mapping. If @x@ is not mapped by any entry in
-- @AssocList@ @l@, then @l '!' x@ throws 'MissingAssocListKey'.
-- The exclamation mark is intended as a reminder of this danger.
--
-- >>> [('A', 1), ('B', 2), ('B', 3), ('C', 4)] ! 'D'
-- *** Exception: MissingAssocListKey
--
-- There is a related operator called '!?' which maps the
-- missing-key condition to 'Nothing' instead.

(!) :: forall l a b. (AssocList l a b, Eq a)
    => l -> a -> b
(uncons -> Nothing) ! key               = throw MissingAssocListKey
(uncons -> Just ((x, y), xys)) ! key
        | key == x                      = y
        | otherwise                     = xys ! key

-- | Obtain the first value associated with a particular key, if such
-- a mapping is present.
--
-- >>> [('A',1), ('B',2), ('B',3), ('C',4)] !? 'B'
-- Just 2
--
-- The result is 'Nothing' if the key is not mapped by any entry in
-- the list.
--
-- >>> [('A',1), ('B',2), ('B',3), ('C',4)] !? 'D'
-- Nothing
--
-- This function is the same as 'lookupFirst' but for the order of
-- its arguments.

(!?) :: forall l a b. (AssocList l a b, Eq a)
    => l -> a -> Maybe b
l !? key = lookupFirst key l

-- | Obtain the first value associated with a particular key, if such
-- a mapping is present.
--
-- >>> lookupFirst 'B' [('A',1), ('B',2), ('B',3), ('C',4)]
-- Just 2
--
-- The result is 'Nothing' if the key is not mapped by any entry in
-- the list.
--
-- >>> lookupFirst 'D' [('A',1), ('B',2), ('B',3), ('C',4)]
-- Nothing
--
-- This function is the same as '!?' but for the order of its
-- arguments.

lookupFirst :: forall l a b. (AssocList l a b, Eq a)
    => a -> l -> Maybe b
lookupFirst _key (uncons -> Nothing)    =  Nothing
lookupFirst key (uncons -> Just ((x, y), xys))
        | key == x                      =  Just y
        | otherwise                     =  lookupFirst key xys

-- | Obtain all values associated with a particular key, in the
-- order in which the mappings appear in the list.
--
-- >>> lookupAll 'B' [('A',1), ('B',2), ('B',3), ('C',4), ('B',3)]
-- [2,3,3]

lookupAll :: forall l a b. (AssocList l a b, Eq a)
    => a -> l -> [b]
lookupAll _key (uncons -> Nothing)      =  []
lookupAll key (uncons -> Just ((x, y), xys))
        | key == x                      =  y : lookupAll key xys
        | otherwise                     =      lookupAll key xys

-- | Produce a modified version of the association list in which the
-- first occurrence of a particular key has been removed.
--
-- >>> removeFirst 'B' [('A',1), ('B',2), ('B',3), ('C',4)]
-- [('A',1),('B',3),('C',4)]
--
-- If the key is not present in the mapping, then the original list
-- is returned.
--
-- >>> removeFirst 'C' [('A',1), ('B',2), ('B',3)]
-- [('A',1),('B',2),('B',3)]

removeFirst :: forall l a b. (AssocList l a b, Eq a)
    => a -> l -> l
removeFirst _key l@(uncons -> Nothing)  =  l
removeFirst key (uncons -> Just (xy@(x, y), xys))
        | key == x                      =  xys
        | otherwise                     =  cons xy (removeFirst key xys)

-- | Produce a modified version of the association list in which all
-- occurrences of a particular key have been removed.
--
-- >>> removeAll 'B' [('A',1), ('B',2), ('B',3), ('C',4)]
-- [('A',1),('C',4)]
--
-- If the key is not present in the mapping, then the original list
-- is returned.
--
-- >>> removeAll 'C' [('A',1), ('B',2), ('B',3)]
-- [('A',1),('B',2),('B',3)]

removeAll :: forall l a b. (AssocList l a b, Eq a)
    => a -> l -> l
removeAll _key l@(uncons -> Nothing)    =  l
removeAll key (uncons -> Just (xy@(x, y), xys))
        | key == x                      =       removeAll key xys
        | otherwise                     =  cons xy (removeAll key xys)

-- | Produces a tuple of two results:
--
-- 1. All values associated with a particular key
-- 2. All of the other key-value pairs
--
-- @'partition' x l = ('lookupAll' x l, 'removeAll' x l)@
--
-- >>> partition 'B' [('A',1), ('B',2), ('B',3), ('C',4), ('B',3)]
-- ([2,3,3],[('A',1),('C',4)])

partition :: forall l a b. (AssocList l a b, Eq a)
    => a -> l -> ([b], l)
partition _key l@(uncons -> Nothing)    = ([], l)
partition key (uncons -> Just (xy@(x, y), xys))
        | key == x                      = (y : yes ,         no)
        | otherwise                     = (    yes , cons xy no)
    where
        (yes, no) = partition key xys

-- | Produces a tuple of two results:
--
-- 1. The longest prefix of the association list that does /not/ contain
--    a particular key
-- 2. The remainder of the list
--
-- >>> break 'B' [('A',1), ('B',2), ('B',3), ('C',4)]
-- ([('A',1)],[('B',2),('B',3),('C',4)])
--
-- If the first mapping in the list contains the given key, then the first
-- part of the resulting tuple is empty, and the second part of the result
-- is the entire list.
--
-- >>> break 'A' [('A',1), ('B',2), ('B',3), ('C',4)]
-- ([],[('A',1),('B',2),('B',3),('C',4)])
--
-- If the key is not present in the list, then the first part of the
-- resulting tuple is the entire list, and the second part of the result
-- is empty.
--
-- >>> break 'D' [('A',1), ('B',2), ('B',3), ('C',4)]
-- ([('A',1),('B',2),('B',3),('C',4)],[])

break :: forall l a b. (AssocList l a b, Eq a)
    => a -> l -> (l, l)
break key = LL.break (\(x, y) -> key == x)

-- | 'break' on a key, then 'partition' the remainder.
--
-- @'breakPartition' key l@ separates @l@ into three parts:
--
-- 1. The key-value pairs for which the key is /not/ @key@ that
--    occur in the list /before/ the first occurrence of @key@
--    (@fst ('break' key l)@)
-- 2. All values associated with @key@ (@'lookupAll' key l@)
-- 3. The key-value pairs for which the key is /not/ @key@ that
--    occur in the list /after/ the first occurrence of @key@
--    (@'removeAll' key (snd ('break' key l))@)
--
-- >>> breakPartition 'B' [('A',1),('B',2),('C',3),('B',4)]
-- ([('A',1)],[2,4],[('C',3)])
--
-- If the key is not present in the list, then the first part of the
-- result is the entire list, and the other parts are empty.
--
-- >>> breakPartition 'D' [('A',1),('B',2),('C',3),('B',4)]
-- ([('A',1),('B',2),('C',3),('B',4)],[],[])

breakPartition :: forall l a b. (AssocList l a b, Eq a)
    => a -> l -> (l, [b], l)
breakPartition key l =
    let
        (before, l') = break     key l
        (xs, after)  = partition key l'
    in
        (before, xs, after)

-- $mapping
-- The "map" functions modify values while preserving the structure of
-- the assocative list. The resulting list has the same size and order
-- as the original.

-- | At the position where a particular key first appears in the list,
-- apply a function to the corresponding value.
--
-- >>> mapFirst 'B' negate [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('B',-4),('C',2),('B',6)]
--
-- If the key does not appear in the list, then the original list is
-- returned without modification.
--
-- >>> mapFirst 'D' negate [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('B',4),('C',2),('B',6)]

mapFirst :: forall l a b. (AssocList l a b, Eq a)
    => a -> (b -> b) -> l -> l
mapFirst key f l =
    let
        (before, l') = break key l
    in
        before `LL.append`
        case (uncons l') of
            Nothing               ->  l'
            Just ((x, y), after)  ->  cons (x, f y) after

-- | At each position where a particular key appears in the list,
-- apply a function to the corresponding value.
--
-- >>> mapAll 'B' negate [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('B',-4),('C',2),('B',-6)]
--
-- If the key does not appear in the list, then the original list is
-- returned without modification.
--
-- >>> mapAll 'D' negate [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('B',4),('C',2),('B',6)]

mapAll :: forall l a b. (AssocList l a b, Eq a)
    => a -> (b -> b) -> l -> l
mapAll key f =
    LL.map g
  where
    g xy@(x, y)
        | key == x   =  (x, f y)
        | otherwise  =  xy

-- $alteration
-- The "alter" functions provide an all-in-one way to do insertion,
-- modification, and removal.

-- | Insert, modify, or delete a single value corresponding to
-- the first place where a particular key appears in the list.
--
-- __Modification__ - If the key first appears in the list with a
-- corresponding value of @x@, and @f x = 'Just' x'@, then that value
-- @x@ will be replaced with @x'@ in the resulting list.
--
-- >>> alterFirst 'B' (fmap negate) [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('B',-4),('C',2),('B',6)]
--
-- __Removal__ - If the key first appears in the list with a corresponding
-- value of @x@, and @f x = 'Nothing'@, then that mapping will be removed
-- in the resulting list.
--
-- >>> alterFirst 'B' (\_ -> Nothing) [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('C',2),('B',6)]
--
-- __Insertion__ - If the key does not appear in the list and
-- @f 'Nothing' = 'Just' x@, then @x@ be appended to the /end/ of the list.
--
-- >>> alterFirst 'D' (\_ -> Just 0) [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('B',4),('C',2),('B',6),('D',0)]

alterFirst :: forall l a b. (AssocList l a b, Eq a)
    => a -> (Maybe b -> Maybe b) -- ^ @f@
    -> l -> l
alterFirst key f l =
    let (before, l') = break key l
    in  before `LL.append`
        case LL.uncons l' of
            Nothing               ->  maybe LL.empty LL.singleton ((,) key <$> f Nothing)
            Just ((x, y), after)  ->  maybe LL.empty LL.singleton ((,) x   <$> f (Just y))
                                      `LL.append` after

-- | Modify the list of values that correspond to a particular key.
--
-- __Mapping__ - For example, to negate all values of @'B'@:
--
-- >>> alterAll 'B' (map negate) [('A', 1), ('B', 4), ('B', 5), ('C', 2)]
-- [('A',1),('B',-4),('B',-5),('C',2)]
--
-- __Length alteration__ - For example, to limit the number of occurrences
-- of 'B' to at most two:
--
-- >>> alterAll 'B' (take 2) [('A', 1), ('B', 4), ('B', 5), ('B', 6), ('C', 2)]
-- [('A',1),('B',4),('B',5),('C',2)]
--
-- __Removal__ - If @f@ returns an empty list, then the key will be removed
-- from the list entirely.
--
-- >>> alterAll 'B' (\_ -> []) [('A', 1), ('B', 4), ('B', 5), ('C', 2)]
-- [('A',1),('C',2)]
--
-- __Reordering__ - The key may appear in multiple noncontiguous positions
-- in the input list, but all of the new mappings for the key in the output
-- will be in one contiguous sequence starting at the position where the
-- key /first/ appears in the input list.
--
-- >>> alterAll 'B' (map negate) [('A', 1), ('B', 4), ('C', 2), ('B', 5), ('D', 3), ('B', 6)]
-- [('A',1),('B',-4),('B',-5),('B',-6),('C',2),('D',3)]
--
-- __Insertion__ - If the key does not appear in the list, then any result
-- from @f@ will be appended to the /end/ of the list.
--
-- >>> alterAll 'D' (\_ -> [7, 8]) [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('B',4),('C',2),('B',6),('D',7),('D',8)]

alterAll :: forall l a b. (AssocList l a b, Eq a)
    => a -> ([b] -> [b]) -- ^ @f@
    -> l -> l
alterAll key f l =
    let (before, l') = break key l
    in  before `LL.append`
        case (uncons l') of
            Nothing  ->  LL.fromList ((,) key <$> f [])
            _        ->  let (ys, after) = partition key l'
                         in  LL.fromList ((,) key <$> f ys) `LL.append` after
