module Data.AssocList.List.Predicate where

import Data.AssocList.List.Type

-- base
import qualified Data.List
import Prelude (Maybe (..), otherwise)

-- contravariant
import Data.Functor.Contravariant (Predicate (..))

-- $setup
-- >>> import Prelude ((==))

lookupFirst :: Predicate a -> AssocList a b -> Maybe b
lookupFirst _key []                =  Nothing
lookupFirst key ((x, y) : xys)
        | getPredicate key x       =  Just y
        | otherwise                =  lookupFirst key xys

lookupAll :: Predicate a -> AssocList a b -> [b]
lookupAll _key []                  =  []
lookupAll key ((x, y) : xys)
        | getPredicate key x       =  y : lookupAll key xys
        | otherwise                =      lookupAll key xys

removeFirst :: Predicate a -> AssocList a b -> AssocList a b
removeFirst _key l@[]              =  l
removeFirst key (xy@(x, y) : xys)
        | getPredicate key x       =  xys
        | otherwise                =  xy : removeFirst key xys

removeAll :: Predicate a -> AssocList a b -> AssocList a b
removeAll _key l@[]                =  l
removeAll key (xy@(x, y) : xys)
        | getPredicate key x       =       removeAll key xys
        | otherwise                =  xy : removeAll key xys

-- | @'partition' x l = ('lookupAll' x l, 'removeAll' x l)@
partition :: Predicate a -> AssocList a b -> ([b], AssocList a b)
partition _key l@[]                = ([], l)
partition key (xy@(x, y) : xys)
        | getPredicate key x       = (y : yes ,      no)
        | otherwise                = (    yes , xy : no)
  where
    (yes, no) = partition key xys

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
