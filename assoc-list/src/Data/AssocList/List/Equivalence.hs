module Data.AssocList.List.Equivalence where

import Data.AssocList.Exception
import Data.AssocList.List.Type

-- base
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
