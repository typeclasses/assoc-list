-- | Functions on 'AssocList's that make use of an 'Eq' constraint
-- on the type of the keys.
--
-- Some other modules that are a lot like this one:
--
-- * "Data.AssocList.List.Equivalence"
-- * "Data.AssocList.List.Predicate"

module Data.AssocList.List.Eq where

import Data.AssocList.Exception
import Data.AssocList.List.Type

-- base
import Control.Exception (throw)
import qualified Data.List
import Prelude (Eq (..), Maybe (..), error, otherwise)

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

(!) :: Eq a => AssocList a b -> a -> b
[] ! key                           = throw MissingAssocListKey
((x, y) : xys) ! key
        | key == x                 = y
        | otherwise                = xys ! key

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

(!?) :: Eq a => AssocList a b -> a -> Maybe b
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

lookupFirst :: Eq a => a -> AssocList a b -> Maybe b
lookupFirst _key []                =  Nothing
lookupFirst key ((x, y) : xys)
        | key == x                 =  Just y
        | otherwise                =  lookupFirst key xys

-- | Obtain all values associated with a particular key, in the
-- order in which the mappings appear in the list.
--
-- >>> lookupAll 'B' [('A',1), ('B',2), ('B',3), ('C',4), ('B',3)]
-- [2,3,3]

lookupAll :: Eq a => a -> AssocList a b -> [b]
lookupAll _key []                  =  []
lookupAll key ((x, y) : xys)
        | key == x                 =  y : lookupAll key xys
        | otherwise                =      lookupAll key xys

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

removeFirst :: Eq a => a -> AssocList a b -> AssocList a b
removeFirst _key l@[]              =  l
removeFirst key (xy@(x, y) : xys)
        | key == x                 =  xys
        | otherwise                =  xy : removeFirst key xys

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

removeAll :: Eq a => a -> AssocList a b -> AssocList a b
removeAll _key l@[]                =  l
removeAll key (xy@(x, y) : xys)
        | key == x                 =       removeAll key xys
        | otherwise                =  xy : removeAll key xys

-- | Produces a tuple of two results:
--
-- 1. All values associated with a particular key
-- 2. All of the other key-value pairs
--
-- @'partition' x l = ('lookupAll' x l, 'removeAll' x l)@
--
-- >>> partition 'B' [('A',1), ('B',2), ('B',3), ('C',4), ('B',3)]
-- ([2,3,3],[('A',1),('C',4)])

partition :: Eq a => a -> AssocList a b -> ([b], AssocList a b)
partition _key l@[]                = ([], l)
partition key (xy@(x, y) : xys)
        | key == x                 = (y : yes ,      no)
        | otherwise                = (    yes , xy : no)
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

break :: Eq a => a -> AssocList a b -> (AssocList a b, AssocList a b)
break key = Data.List.break (\(x, y) -> key == x)

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

breakPartition :: Eq a => a -> AssocList a b
    -> (AssocList a b, [b], AssocList a b)
breakPartition key l =
    let
        (before, l') = break     key l
        (xs, after)  = partition key l'
    in
        (before, xs, after)
