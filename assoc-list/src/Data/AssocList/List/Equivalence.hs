-- | Functions on 'AssocList's that involve 'Equivalence's on the keys.

module Data.AssocList.List.Equivalence
    (

    -- * Related modules
    -- $relatedModules

    -- * Lookup
      lookupFirst
    , lookupAll

    -- * Removal
    , removeFirst
    , removeAll

    -- * Grouping
    , partition
    , break
    , breakPartition

    ) where

import Data.AssocList.Exception
import Data.AssocList.List.Type

-- base
import qualified Data.List
import Prelude (Maybe (..), otherwise)

-- contravariant
import Data.Functor.Contravariant (Equivalence (..))

-- $setup
-- >>> import Data.Functor.Contravariant (defaultEquivalence)

-- $relatedModules
-- Some other modules that are a lot like this one:
--
-- * "Data.AssocList.List.Eq" - Functions on 'AssocList's that make
--   use of an 'Eq' constraint on the type of the keys
-- * "Data.AssocList.List.Predicate" - Functions on 'AssocList's
--   that involve 'Predicate's on the keys

-- | Obtain the first value associated with a particular key, if such
-- a mapping is present.
--
-- >>> lookupFirst defaultEquivalence 'B' [('A',1), ('B',2), ('B',3), ('C',4)]
-- Just 2
--
-- The result is 'Nothing' if the key is not mapped by any entry in
-- the list.
--
-- >>> lookupFirst defaultEquivalence 'D' [('A',1), ('B',2), ('B',3), ('C',4)]
-- Nothing
--
-- This function is the same as '!?' but for the order of its
-- arguments.

lookupFirst :: Equivalence a -> a -> AssocList a b -> Maybe b
lookupFirst _eq _key []            =  Nothing
lookupFirst eq key ((x, y) : xys)
        | getEquivalence eq key x  =  Just y
        | otherwise                =  lookupFirst eq key xys

-- | Obtain all values associated with a particular key, in the
-- order in which the mappings appear in the list.
--
-- >>> lookupAll defaultEquivalence 'B' [('A',1), ('B',2), ('B',3), ('C',4), ('B',3)]
-- [2,3,3]

lookupAll :: Equivalence a -> a -> AssocList a b -> [b]
lookupAll _eq _key []              =  []
lookupAll eq key ((x, y) : xys)
        | getEquivalence eq key x  =  y : lookupAll eq key xys
        | otherwise                =      lookupAll eq key xys

-- | Produce a modified version of the association list in which the
-- first occurrence of a particular key has been removed.
--
-- >>> removeFirst defaultEquivalence 'B' [('A',1), ('B',2), ('B',3), ('C',4)]
-- [('A',1),('B',3),('C',4)]
--
-- If the key is not present in the mapping, then the original list
-- is returned.
--
-- >>> removeFirst defaultEquivalence 'C' [('A',1), ('B',2), ('B',3)]
-- [('A',1),('B',2),('B',3)]

removeFirst :: Equivalence a -> a -> AssocList a b -> AssocList a b
removeFirst _eq _key l@[]          =  l
removeFirst eq key (xy@(x, y) : xys)
        | getEquivalence eq key x  =  xys
        | otherwise                =  xy : removeFirst eq key xys

-- | Produce a modified version of the association list in which all
-- occurrences of a particular key have been removed.
--
-- >>> removeAll defaultEquivalence 'B' [('A',1), ('B',2), ('B',3), ('C',4)]
-- [('A',1),('C',4)]
--
-- If the key is not present in the mapping, then the original list
-- is returned.
--
-- >>> removeAll defaultEquivalence 'C' [('A',1), ('B',2), ('B',3)]
-- [('A',1),('B',2),('B',3)]

removeAll :: Equivalence a -> a -> AssocList a b -> AssocList a b
removeAll _eq _key l@[]            =  l
removeAll eq key (xy@(x, y) : xys)
        | getEquivalence eq key x  =       removeAll eq key xys
        | otherwise                =  xy : removeAll eq key xys

-- | Produces a tuple of two results:
--
-- 1. All values associated with a particular key
-- 2. All of the other key-value pairs
--
-- @'partition' eq x l = ('lookupAll' eq x l, 'removeAll' eq x l)@
--
-- >>> partition defaultEquivalence 'B' [('A',1), ('B',2), ('B',3), ('C',4), ('B',3)]
-- ([2,3,3],[('A',1),('C',4)])

partition :: Equivalence a -> a -> AssocList a b -> ([b], AssocList a b)
partition _eq _key l@[]            = ([], l)
partition eq key (xy@(x, y) : xys)
        | getEquivalence eq key x  = (y : yes ,      no)
        | otherwise                = (    yes , xy : no)
  where
    (yes, no) = partition eq key xys

-- | Produces a tuple of two results:
--
-- 1. The longest prefix of the association list that does /not/ contain
--    a particular key
-- 2. The remainder of the list
--
-- >>> break defaultEquivalence 'B' [('A',1), ('B',2), ('B',3), ('C',4)]
-- ([('A',1)],[('B',2),('B',3),('C',4)])
--
-- If the first mapping in the list contains the given key, then the first
-- part of the resulting tuple is empty, and the second part of the result
-- is the entire list.
--
-- >>> break defaultEquivalence 'A' [('A',1), ('B',2), ('B',3), ('C',4)]
-- ([],[('A',1),('B',2),('B',3),('C',4)])
--
-- If the key is not present in the list, then the first part of the
-- resulting tuple is the entire list, and the second part of the result
-- is empty.
--
-- >>> break defaultEquivalence 'D' [('A',1), ('B',2), ('B',3), ('C',4)]
-- ([('A',1),('B',2),('B',3),('C',4)],[])

break :: Equivalence a -> a -> AssocList a b -> (AssocList a b, AssocList a b)
break eq key = Data.List.break (\(x, y) -> getEquivalence eq key x)

-- | 'break' on a key, then 'partition' the remainder.
--
-- @'breakPartition' eq key l@ separates @l@ into three parts:
--
-- 1. The key-value pairs for which the key is /not/ @key@ that
--    occur in the list /before/ the first occurrence of @key@
--    (@fst ('break' eq key l)@)
-- 2. All values associated with @key@ (@'lookupAll' eq key l@)
-- 3. The key-value pairs for which the key is /not/ @key@ that
--    occur in the list /after/ the first occurrence of @key@
--    (@'removeAll' eq key (snd ('break' eq key l))@)
--
-- >>> breakPartition defaultEquivalence 'B' [('A',1),('B',2),('C',3),('B',4)]
-- ([('A',1)],[2,4],[('C',3)])
--
-- If the key is not present in the list, then the first part of the
-- result is the entire list, and the other parts are empty.
--
-- >>> breakPartition defaultEquivalence 'D' [('A',1),('B',2),('C',3),('B',4)]
-- ([('A',1),('B',2),('C',3),('B',4)],[],[])

breakPartition :: Equivalence a -> a -> AssocList a b
    -> (AssocList a b, [b], AssocList a b)
breakPartition eq key l =
    let
        (before, l') = break     eq key l
        (xs, after)  = partition eq key l'
    in
        (before, xs, after)
