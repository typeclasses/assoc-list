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

    ) where

import Data.AssocList.Exception
import Data.AssocList.List.Type

-- base
import qualified Data.List
import Data.Maybe (maybeToList)
import Prelude (Maybe (..), (++), (<$>), otherwise)

-- contravariant
import Data.Functor.Contravariant (Equivalence (..))

-- $setup
-- >>> import Data.Functor.Contravariant (defaultEquivalence)
-- >>> import Prelude (fmap, map, negate, take)

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

-- $mapping
-- The "map" functions modify values while preserving the structure of
-- the assocative list. The resulting list has the same size and order
-- as the original.

-- | At the position where a particular key first appears in the list,
-- apply a function to the corresponding value.
--
-- >>> mapFirst defaultEquivalence 'B' negate [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('B',-4),('C',2),('B',6)]
--
-- If the key does not appear in the list, then the original list is
-- returned without modification.
--
-- >>> mapFirst defaultEquivalence 'D' negate [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('B',4),('C',2),('B',6)]

mapFirst :: Equivalence a -> a -> (b -> b) -> AssocList a b -> AssocList a b
mapFirst eq key f l =
    let
        (before, l') = break eq key l
    in
        before ++
        case l' of
            []              ->  l'
            (x, y) : after  ->  (x, f y) : after

-- | At each position where a particular key appears in the list,
-- apply a function to the corresponding value.
--
-- >>> mapAll defaultEquivalence 'B' negate [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('B',-4),('C',2),('B',-6)]
--
-- If the key does not appear in the list, then the original list is
-- returned without modification.
--
-- >>> mapAll defaultEquivalence 'D' negate [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('B',4),('C',2),('B',6)]

mapAll :: Equivalence a -> a -> (b -> b) -> AssocList a b -> AssocList a b
mapAll eq key f =
    Data.List.map g
  where
    g xy@(x, y)
        | getEquivalence eq key x  =  (x, f y)
        | otherwise                =  xy

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
-- >>> alterFirst defaultEquivalence 'B' (fmap negate) [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('B',-4),('C',2),('B',6)]
--
-- __Removal__ - If the key first appears in the list with a corresponding
-- value of @x@, and @f x = 'Nothing'@, then that mapping will be removed
-- in the resulting list.
--
-- >>> alterFirst defaultEquivalence 'B' (\_ -> Nothing) [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('C',2),('B',6)]
--
-- __Insertion__ - If the key does not appear in the list and
-- @f 'Nothing' = 'Just' x@, then @x@ be appended to the /end/ of the list.
--
-- >>> alterFirst defaultEquivalence 'D' (\_ -> Just 0) [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('B',4),('C',2),('B',6),('D',0)]

alterFirst :: Equivalence a -> a -> (Maybe b -> Maybe b) -- ^ @f@
                            -> AssocList a b -> AssocList a b
alterFirst eq key f l =
    let (before, l') = break eq key l
    in  before ++ case l' of
                      []              ->  maybeToList ((,) key <$> f Nothing)
                      (x, y) : after  ->  maybeToList ((,) x   <$> f (Just y))
                                          ++ after

-- | Modify the list of values that correspond to a particular key.
--
-- __Mapping__ - For example, to negate all values of @'B'@:
--
-- >>> alterAll defaultEquivalence 'B' (map negate) [('A', 1), ('B', 4), ('B', 5), ('C', 2)]
-- [('A',1),('B',-4),('B',-5),('C',2)]
--
-- __Length alteration__ - For example, to limit the number of occurrences
-- of 'B' to at most two:
--
-- >>> alterAll defaultEquivalence 'B' (take 2) [('A', 1), ('B', 4), ('B', 5), ('B', 6), ('C', 2)]
-- [('A',1),('B',4),('B',5),('C',2)]
--
-- __Removal__ - If @f@ returns an empty list, then the key will be removed
-- from the list entirely.
--
-- >>> alterAll defaultEquivalence 'B' (\_ -> []) [('A', 1), ('B', 4), ('B', 5), ('C', 2)]
-- [('A',1),('C',2)]
--
-- __Reordering__ - The key may appear in multiple noncontiguous positions
-- in the input list, but all of the new mappings for the key in the output
-- will be in one contiguous sequence starting at the position where the
-- key /first/ appears in the input list.
--
-- >>> alterAll defaultEquivalence 'B' (map negate) [('A', 1), ('B', 4), ('C', 2), ('B', 5), ('D', 3), ('B', 6)]
-- [('A',1),('B',-4),('B',-5),('B',-6),('C',2),('D',3)]
--
-- __Insertion__ - If the key does not appear in the list, then any result
-- from @f@ will be appended to the /end/ of the list.
--
-- >>> alterAll defaultEquivalence 'D' (\_ -> [7, 8]) [('A', 1), ('B', 4), ('C', 2), ('B', 6)]
-- [('A',1),('B',4),('C',2),('B',6),('D',7),('D',8)]

alterAll :: Equivalence a -> a -> ([b] -> [b]) -- ^ @f@
                          -> AssocList a b -> AssocList a b
alterAll eq key f l =
    let (before, l') = break eq key l
    in  before ++ case l' of
                      []  ->  (,) key <$> f []
                      _   ->  let (ys, after) = partition eq key l'
                              in  ((,) key <$> f ys) ++ after
