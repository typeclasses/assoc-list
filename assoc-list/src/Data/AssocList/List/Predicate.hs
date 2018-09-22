-- | Functions on 'AssocList's that involve 'Predicate's on the keys.

module Data.AssocList.List.Predicate
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

    -- * Grouping
    , partition
    , break
    , breakPartition

    ) where

import Data.AssocList.List.Concept

-- base
import qualified Data.List
import Data.Maybe (maybeToList)
import Prelude (Maybe (..), (++), (<$>), otherwise)

-- contravariant
import Data.Functor.Contravariant (Predicate (..))

-- $setup
-- >>> import Prelude ((==), negate)

-- $relatedModules
-- Some other modules that are a lot like this one:
--
-- * "Data.AssocList.List.Eq" - Functions on 'AssocList's that make
--   use of an 'Eq' constraint on the type of the keys
-- * "Data.AssocList.List.Equivalence" - Functions on 'AssocList's
--   that involve 'Equivalence's on the keys

-- | Obtain the first value associated with a key that satisfies a
-- predicate, if such a mapping is present.
--
-- >>> lookupFirst (Predicate (== 'B')) [('A',1), ('B',2), ('B',3), ('C',4)]
-- Just 2
--
-- The result is 'Nothing' if no key in the list satisfies the predicate.
--
-- >>> lookupFirst (Predicate (== 'D')) [('A',1), ('B',2), ('B',3), ('C',4)]
-- Nothing

lookupFirst :: Predicate a -> AssocList a b -> Maybe b
lookupFirst _key []                =  Nothing
lookupFirst key ((x, y) : xys)
        | getPredicate key x       =  Just y
        | otherwise                =  lookupFirst key xys

-- | Obtain all values associated with keys that satisfy the predicate,
-- in the order in which the mappings appear in the list.
--
-- >>> lookupAll (Predicate (== 'B')) [('A',1), ('B',2), ('B',3), ('C',4), ('B',3)]
-- [2,3,3]

lookupAll :: Predicate a -> AssocList a b -> [b]
lookupAll _key []                  =  []
lookupAll key ((x, y) : xys)
        | getPredicate key x       =  y : lookupAll key xys
        | otherwise                =      lookupAll key xys

-- | Produce a modified version of the association list in which the
-- first occurrence of a key that satisfied the predicate has been removed.
--
-- >>> removeFirst (Predicate (== 'B')) [('A',1), ('B',2), ('B',3), ('C',4)]
-- [('A',1),('B',3),('C',4)]
--
-- If no key in the list satisfies the predicate, then the original list
-- is returned.
--
-- >>> removeFirst (Predicate (== 'C')) [('A',1), ('B',2), ('B',3)]
-- [('A',1),('B',2),('B',3)]

removeFirst :: Predicate a -> AssocList a b -> AssocList a b
removeFirst _key l@[]              =  l
removeFirst key (xy@(x, y) : xys)
        | getPredicate key x       =  xys
        | otherwise                =  xy : removeFirst key xys

-- | Produce a modified version of the association list in which all
-- occurrences of keys that satisfy the predicate have been removed.
--
-- >>> removeAll (Predicate (== 'B')) [('A',1), ('B',2), ('B',3), ('C',4)]
-- [('A',1),('C',4)]
--
-- If the key is not present in the mapping, then the original list
-- is returned.
--
-- >>> removeAll (Predicate (== 'C')) [('A',1), ('B',2), ('B',3)]
-- [('A',1),('B',2),('B',3)]

removeAll :: Predicate a -> AssocList a b -> AssocList a b
removeAll _key l@[]                =  l
removeAll key (xy@(x, y) : xys)
        | getPredicate key x       =       removeAll key xys
        | otherwise                =  xy : removeAll key xys

-- | Produces a tuple of two results:
--
-- 1. All values associated with keys that satify the predicate
-- 2. All of the other key-value pairs
--
-- @'partition' x l = ('lookupAll' x l, 'removeAll' x l)@
--
-- >>> partition (Predicate (== 'B')) [('A',1), ('B',2), ('B',3), ('C',4), ('B',3)]
-- ([2,3,3],[('A',1),('C',4)])

partition :: Predicate a -> AssocList a b -> ([b], AssocList a b)
partition _key l@[]                = ([], l)
partition key (xy@(x, y) : xys)
        | getPredicate key x       = (y : yes ,      no)
        | otherwise                = (    yes , xy : no)
    where
        (yes, no) = partition key xys

-- | Produces a tuple of two results:
--
-- 1. The longest prefix of the association list that does /not/ contain
--    a key satisfying the predict
-- 2. The remainder of the list
--
-- >>> break (Predicate (== 'B')) [('A',1), ('B',2), ('B',3), ('C',4)]
-- ([('A',1)],[('B',2),('B',3),('C',4)])
--
-- If the key of the first mapping in the list satisfies the predicate,
-- then the first part of the resulting tuple is empty, and the second
-- part of the result is the entire list.
--
-- >>> break (Predicate (== 'A')) [('A',1), ('B',2), ('B',3), ('C',4)]
-- ([],[('A',1),('B',2),('B',3),('C',4)])
--
-- If no key in the list satisfies the predicate, then the first part of
-- the resulting tuple is the entire list, and the second part of the
-- result is empty.
--
-- >>> break (Predicate (== 'D')) [('A',1), ('B',2), ('B',3), ('C',4)]
-- ([('A',1),('B',2),('B',3),('C',4)],[])

break :: Predicate a -> AssocList a b -> (AssocList a b, AssocList a b)
break key = Data.List.break (\(x, y) -> getPredicate key x)

-- | 'break' on a predicate, then 'partition' the remainder.
--
-- @'breakPartition' p l@ separates @l@ into three parts:
--
-- 1. The key-value pairs for which the predicate is /not/ satisfied that
--    occur in the list /before/ the first occurrence of a key that satisfies
--    the predicate (@fst ('break' p l)@)
-- 2. All values associated with keys that satisfy the predicate
--    (@'lookupAll' p l@)
-- 3. The key-value pairs for which the predicate is /not/ satisfied that
--    occur in the list /after/ the first occurrence of a key that satisfies
--    the predicate (@'removeAll' p (snd ('break' p l))@)
--
-- >>> breakPartition (Predicate (== 'B')) [('A',1),('B',2),('C',3),('B',4)]
-- ([('A',1)],[2,4],[('C',3)])
--
-- If the predicate is not satisfied by any key in the list, then the
-- first part of the result is the entire list, and the other parts are
-- empty.
--
-- >>> breakPartition (Predicate (== 'D')) [('A',1),('B',2),('C',3),('B',4)]
-- ([('A',1),('B',2),('C',3),('B',4)],[],[])

breakPartition :: Predicate a -> AssocList a b
    -> (AssocList a b, [b], AssocList a b)
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

-- | At the position where a key satisfying the predicate first appears
-- in the list, apply a function to the corresponding value.
--
-- >>> mapFirst (Predicate (== 'B')) negate [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('B',-4),('C',2),('B',6)]
--
-- If no key in the list satisfies the predicate, then the original list is
-- returned without modification.
--
-- >>> mapFirst (Predicate (== 'D')) negate [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('B',4),('C',2),('B',6)]

mapFirst :: Predicate a -> (b -> b) -> AssocList a b -> AssocList a b
mapFirst key f l =
    let
        (before, l') = break key l
    in
        before ++
        case l' of
            []              ->  l'
            (x, y) : after  ->  (x, f y) : after

-- | At each position in the list where the key satisfies the predicate,
-- apply a function to the corresponding value.
--
-- >>> mapAll (Predicate (== 'B')) negate [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('B',-4),('C',2),('B',-6)]
--
-- If no key in the list satisfies the predicate, then the original list is
-- returned without modification.
--
-- >>> mapAll (Predicate (== 'D')) negate [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('B',4),('C',2),('B',6)]

mapAll :: Predicate a -> (b -> b) -> AssocList a b -> AssocList a b
mapAll key f =
    Data.List.map g
  where
    g xy@(x, y)
        | getPredicate key x   =  (x, f y)
        | otherwise            =  xy
