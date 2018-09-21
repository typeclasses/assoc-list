module Data.AssocList.List.Equivalence where

import Data.AssocList.Exception
import Data.AssocList.List.Type

-- base
import qualified Data.List
import Prelude (Maybe (..), otherwise)

-- contravariant
import Data.Functor.Contravariant (Equivalence (..))

lookupFirst :: Equivalence a -> a -> AssocList a b -> Maybe b
lookupFirst _eq _key []            =  Nothing
lookupFirst eq key ((x, y) : xys)
        | getEquivalence eq key x  =  Just y
        | otherwise                =  lookupFirst eq key xys

lookupAll :: Equivalence a -> a -> AssocList a b -> [b]
lookupAll _eq _key []              =  []
lookupAll eq key ((x, y) : xys)
        | getEquivalence eq key x  =  y : lookupAll eq key xys
        | otherwise                =      lookupAll eq key xys

removeFirst :: Equivalence a -> a -> AssocList a b -> AssocList a b
removeFirst _eq _key l@[]          =  l
removeFirst eq key (xy@(x, y) : xys)
        | getEquivalence eq key x  =  xys
        | otherwise                =  xy : removeFirst eq key xys

removeAll :: Equivalence a -> a -> AssocList a b -> AssocList a b
removeAll _eq _key l@[]            =  l
removeAll eq key (xy@(x, y) : xys)
        | getEquivalence eq key x  =       removeAll eq key xys
        | otherwise                =  xy : removeAll eq key xys

-- | @'partition' eq x l = ('lookupAll' eq x l, 'removeAll' eq x l)@
partition :: Equivalence a -> a -> AssocList a b -> ([b], AssocList a b)
partition _eq _key l@[]            = ([], l)
partition eq key (xy@(x, y) : xys)
        | getEquivalence eq key x  = (y : yes ,      no)
        | otherwise                = (    yes , xy : no)
  where
    (yes, no) = partition eq key xys

break :: Equivalence a -> a -> AssocList a b -> (AssocList a b, AssocList a b)
break eq key = Data.List.break (\(x, y) -> getEquivalence eq key x)

-- | 'break' on a key, then 'partition' the remainder.
--
-- @'breakPartition' eq key l@ separates @l@ into three parts:
--
-- 1. The key-value pairs for which the key is /not/ @key@ that
--    occur in the list *before* the first occurrence of @key@
--    (@fst ('break' eq key l)@)
-- 2. All values associated with @key@ (@'lookupAll' eq key l@)
-- 3. The key-value pairs for which the key is /not/ @key@ that
--    occur in the list *after* the first occurrence of @key@
--    (@'removeAll' eq key (snd ('break' eq key l))@)
breakPartition :: Equivalence a -> a -> AssocList a b
    -> (AssocList a b, [b], AssocList a b)
breakPartition eq key l =
    let
        (before, l') = break     eq key l
        (xs, after)  = partition eq key l'
    in
        (before, xs, after)
