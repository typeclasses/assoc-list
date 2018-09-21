module Data.AssocList.List.Eq where

import Data.AssocList.Exception
import Data.AssocList.List.Type

-- base
import Control.Exception (throw)
import qualified Data.List
import Prelude (Eq (..), Maybe (..), error, otherwise)

(!) :: Eq a => AssocList a b -> a -> b
[] ! key                           = throw MissingAssocListKey
((x, y) : xys) ! key
        | key == x                 = y
        | otherwise                = xys ! key

(!?) :: Eq a => AssocList a b -> a -> Maybe b
[] !? key                          = Nothing
((x, y) : xys) !? key
        | key == x                 = Just y
        | otherwise                = xys !? key

lookupFirst :: Eq a => a -> AssocList a b -> Maybe b
lookupFirst _key []                =  Nothing
lookupFirst key ((x, y) : xys)
        | key == x                 =  Just y
        | otherwise                =  lookupFirst key xys

lookupAll :: Eq a => a -> AssocList a b -> [b]
lookupAll _key []                  =  []
lookupAll key ((x, y) : xys)
        | key == x                 =  y : lookupAll key xys
        | otherwise                =      lookupAll key xys

removeFirst :: Eq a => a -> AssocList a b -> AssocList a b
removeFirst _key l@[]              =  l
removeFirst key (xy@(x, y) : xys)
        | key == x                 =  xys
        | otherwise                =  xy : removeFirst key xys

removeAll :: Eq a => a -> AssocList a b -> AssocList a b
removeAll _key l@[]                =  l
removeAll key (xy@(x, y) : xys)
        | key == x                 =       removeAll key xys
        | otherwise                =  xy : removeAll key xys

-- | @'partition' x l = ('lookupAll' x l, 'removeAll' x l)@
partition :: Eq a => a -> AssocList a b -> ([b], AssocList a b)
partition _key l@[]                = ([], l)
partition key (xy@(x, y) : xys)
        | key == x                 = (y : yes ,      no)
        | otherwise                = (    yes , xy : no)
  where
    (yes, no) = partition key xys

break :: Eq a => a -> AssocList a b -> (AssocList a b, AssocList a b)
break key = Data.List.break (\(x, y) -> x == key)
