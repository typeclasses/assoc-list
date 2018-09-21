module Data.AssocList.List.Predicate where

import Data.AssocList.List.Type

-- base
import qualified Data.List
import Prelude (Maybe (..), otherwise)

-- contravariant
import Data.Functor.Contravariant (Predicate (..))

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
