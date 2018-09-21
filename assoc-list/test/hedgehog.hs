{-# LANGUAGE TemplateHaskell #-}

import           Data.AssocList.Exception
import           Data.AssocList.List.Type
import qualified Data.AssocList.List.Comparison
import qualified Data.AssocList.List.Eq
import qualified Data.AssocList.List.Equivalence
import qualified Data.AssocList.List.Ord
import qualified Data.AssocList.List.Predicate

-- base
import           GHC.Stack (HasCallStack)
import           Control.Exception (try, Exception)
import           Control.Monad  (unless)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Foldable  (for_)
import qualified System.Exit    as Exit
import qualified System.IO      as IO

-- contravariant
import qualified Data.Functor.Contravariant
import           Data.Functor.Contravariant
    (Comparison (..), Equivalence (..), Predicate (..))

-- hedgehog
import           Hedgehog     (Property, forAll, property,
                               withTests, (===), MonadTest)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen

main :: IO ()
main = do
    for_ [IO.stdout, IO.stderr] $ \h -> do
        IO.hSetEncoding h IO.utf8
        IO.hSetBuffering h IO.LineBuffering
    success <- Hedgehog.checkParallel $$(Hedgehog.discover)
    unless success Exit.exitFailure

throws
  :: ( MonadIO m, MonadTest m
     , Eq a, Show a
     , Eq e, Exception e
     , HasCallStack
     ) => a -> e -> m ()
throws a e =
  do
    result <- liftIO (try (return $! a))
    result === Left e


--------------------------------------------------------------------------------
--  Data.AssocList.List.Comparison
--------------------------------------------------------------------------------

prop_list_comparison_sortKeys :: Property
prop_list_comparison_sortKeys = withTests 1 $ property $ do

    let
        sortKeys = Data.AssocList.List.Comparison.sortKeys
        def = Data.Functor.Contravariant.defaultComparison

    sortKeys def [(2, 'b'), (3, 'c'), (2, 'a'), (7, 'd'), (2, 'e'), (1, 'f')]
      === [(1, 'f'), (2, 'b'), (2, 'a'), (2, 'e'), (3, 'c'), (7, 'd')]


--------------------------------------------------------------------------------
--  Data.AssocList.List.Eq
--------------------------------------------------------------------------------

prop_list_eq_bang :: Property
prop_list_eq_bang = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        (!) = (Data.AssocList.List.Eq.!)

    l ! 1 === 'a'
    l ! 2 === 'b'
    l ! 3 === 'c'
    throws (l ! 4) MissingAssocListKey

prop_list_eq_bang_maybe :: Property
prop_list_eq_bang_maybe = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        (!?) = (Data.AssocList.List.Eq.!?)

    l !? 1 === Just 'a'
    l !? 2 === Just 'b'
    l !? 3 === Just 'c'
    l !? 4 === Nothing

prop_list_eq_lookupFirst :: Property
prop_list_eq_lookupFirst = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        lookupFirst = Data.AssocList.List.Eq.lookupFirst

    lookupFirst 1 l === Just 'a'
    lookupFirst 2 l === Just 'b'
    lookupFirst 3 l === Just 'c'
    lookupFirst 4 l === Nothing

prop_list_eq_lookupAll :: Property
prop_list_eq_lookupAll = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        lookupAll = Data.AssocList.List.Eq.lookupAll

    lookupAll 1 l === ['a']
    lookupAll 2 l === ['b', 'x']
    lookupAll 3 l === ['c']
    lookupAll 4 l === []

prop_list_eq_removeFirst :: Property
prop_list_eq_removeFirst = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        removeFirst = Data.AssocList.List.Eq.removeFirst

    removeFirst 1 l === [(2, 'b'), (2, 'x'), (3, 'c')]
    removeFirst 2 l === [(1, 'a'), (2, 'x'), (3, 'c')]
    removeFirst 3 l === [(1, 'a'), (2, 'b'), (2, 'x')]
    removeFirst 4 l === [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]

prop_list_eq_removeAll :: Property
prop_list_eq_removeAll = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        removeAll = Data.AssocList.List.Eq.removeAll

    removeAll 1 l === [(2, 'b'), (2, 'x'), (3, 'c')]
    removeAll 2 l === [(1, 'a'), (3, 'c')]
    removeAll 3 l === [(1, 'a'), (2, 'b'), (2, 'x')]
    removeAll 4 l === [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]

prop_list_eq_partition :: Property
prop_list_eq_partition = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        partition = Data.AssocList.List.Eq.partition
        (*) = (,)

    partition 1 l === ['a']      * [(2, 'b'), (2, 'x'), (3, 'c')]
    partition 2 l === ['b', 'x'] * [(1, 'a'), (3, 'c')]
    partition 3 l === ['c']      * [(1, 'a'), (2, 'b'), (2, 'x')]
    partition 4 l === []         * [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]

prop_list_eq_break :: Property
prop_list_eq_break = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (4, 'b'), (3, 'c'), (4, 'd')]
        break = Data.AssocList.List.Eq.break
        (*) = (,)

    break 1 l === [] * [(1, 'a'),    (4, 'b'),    (3, 'c'), (4, 'd')]
    break 2 l === [     (1, 'a'),    (4, 'b'),    (3, 'c'), (4, 'd')] * []
    break 3 l === [     (1, 'a'),    (4, 'b')] * [(3, 'c'), (4, 'd')]
    break 4 l === [     (1, 'a')] * [(4, 'b'),    (3, 'c'), (4, 'd')]


--------------------------------------------------------------------------------
--  Data.AssocList.List.Equivalence
--------------------------------------------------------------------------------

prop_list_equivalence_lookupFirst :: Property
prop_list_equivalence_lookupFirst = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        lookupFirst = Data.AssocList.List.Equivalence.lookupFirst
        def = Data.Functor.Contravariant.defaultEquivalence

    lookupFirst def 1 l === Just 'a'
    lookupFirst def 2 l === Just 'b'
    lookupFirst def 3 l === Just 'c'
    lookupFirst def 4 l === Nothing

prop_list_equivalence_lookupAll :: Property
prop_list_equivalence_lookupAll = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        lookupAll = Data.AssocList.List.Equivalence.lookupAll
        def = Data.Functor.Contravariant.defaultEquivalence

    lookupAll def 1 l === ['a']
    lookupAll def 2 l === ['b', 'x']
    lookupAll def 3 l === ['c']
    lookupAll def 4 l === []

prop_list_equivalence_removeFirst :: Property
prop_list_equivalence_removeFirst = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        removeFirst = Data.AssocList.List.Equivalence.removeFirst
        def = Data.Functor.Contravariant.defaultEquivalence

    removeFirst def 1 l === [(2, 'b'), (2, 'x'), (3, 'c')]
    removeFirst def 2 l === [(1, 'a'), (2, 'x'), (3, 'c')]
    removeFirst def 3 l === [(1, 'a'), (2, 'b'), (2, 'x')]
    removeFirst def 4 l === [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]

prop_list_equivalence_removeAll :: Property
prop_list_equivalence_removeAll = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        removeAll = Data.AssocList.List.Equivalence.removeAll
        def = Data.Functor.Contravariant.defaultEquivalence

    removeAll def 1 l === [(2, 'b'), (2, 'x'), (3, 'c')]
    removeAll def 2 l === [(1, 'a'), (3, 'c')]
    removeAll def 3 l === [(1, 'a'), (2, 'b'), (2, 'x')]
    removeAll def 4 l === [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]

prop_list_equivalence_partition :: Property
prop_list_equivalence_partition = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        partition = Data.AssocList.List.Equivalence.partition
        def = Data.Functor.Contravariant.defaultEquivalence
        (*) = (,)

    partition def 1 l === ['a']      * [(2, 'b'), (2, 'x'), (3, 'c')]
    partition def 2 l === ['b', 'x'] * [(1, 'a'), (3, 'c')]
    partition def 3 l === ['c']      * [(1, 'a'), (2, 'b'), (2, 'x')]
    partition def 4 l === []         * [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]

prop_list_equivalence_break :: Property
prop_list_equivalence_break = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (4, 'b'), (3, 'c'), (4, 'd')]
        break = Data.AssocList.List.Equivalence.break
        def = Data.Functor.Contravariant.defaultEquivalence
        (*) = (,)

    break def 1 l === [] * [(1, 'a'),    (4, 'b'),    (3, 'c'), (4, 'd')]
    break def 2 l === [     (1, 'a'),    (4, 'b'),    (3, 'c'), (4, 'd')] * []
    break def 3 l === [     (1, 'a'),    (4, 'b')] * [(3, 'c'), (4, 'd')]
    break def 4 l === [     (1, 'a')] * [(4, 'b'),    (3, 'c'), (4, 'd')]


--------------------------------------------------------------------------------
--  Data.AssocList.List.Ord
--------------------------------------------------------------------------------

prop_list_ord_sortKeys :: Property
prop_list_ord_sortKeys = withTests 1 $ property $ do

    let
        sortKeys = Data.AssocList.List.Ord.sortKeys

    sortKeys [(2, 'b'), (3, 'c'), (2, 'a'), (7, 'd'), (2, 'e'), (1, 'f')]
      === [(1, 'f'), (2, 'b'), (2, 'a'), (2, 'e'), (3, 'c'), (7, 'd')]


--------------------------------------------------------------------------------
--  Data.AssocList.List.Predicate
--------------------------------------------------------------------------------

prop_list_predicate_lookupFirst :: Property
prop_list_predicate_lookupFirst = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        lookupFirst = Data.AssocList.List.Predicate.lookupFirst
        eq x = Predicate (== x)

    lookupFirst (eq 1) l === Just 'a'
    lookupFirst (eq 2) l === Just 'b'
    lookupFirst (eq 3) l === Just 'c'
    lookupFirst (eq 4) l === Nothing

prop_list_predicate_lookupAll :: Property
prop_list_predicate_lookupAll = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        lookupAll = Data.AssocList.List.Predicate.lookupAll
        eq x = Predicate (== x)

    lookupAll (eq 1) l === ['a']
    lookupAll (eq 2) l === ['b', 'x']
    lookupAll (eq 3) l === ['c']
    lookupAll (eq 4) l === []

prop_list_predicate_removeFirst :: Property
prop_list_predicate_removeFirst = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        removeFirst = Data.AssocList.List.Predicate.removeFirst
        eq x = Predicate (== x)

    removeFirst (eq 1) l === [(2, 'b'), (2, 'x'), (3, 'c')]
    removeFirst (eq 2) l === [(1, 'a'), (2, 'x'), (3, 'c')]
    removeFirst (eq 3) l === [(1, 'a'), (2, 'b'), (2, 'x')]
    removeFirst (eq 4) l === [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]

prop_list_predicate_removeAll :: Property
prop_list_predicate_removeAll = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        removeAll = Data.AssocList.List.Predicate.removeAll
        eq x = Predicate (== x)

    removeAll (eq 1) l === [(2, 'b'), (2, 'x'), (3, 'c')]
    removeAll (eq 2) l === [(1, 'a'), (3, 'c')]
    removeAll (eq 3) l === [(1, 'a'), (2, 'b'), (2, 'x')]
    removeAll (eq 4) l === [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]

prop_list_predicate_partition :: Property
prop_list_predicate_partition = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        partition = Data.AssocList.List.Predicate.partition
        eq x = Predicate (== x)
        (*) = (,)

    partition (eq 1) l === ['a']      * [(2, 'b'), (2, 'x'), (3, 'c')]
    partition (eq 2) l === ['b', 'x'] * [(1, 'a'), (3, 'c')]
    partition (eq 3) l === ['c']      * [(1, 'a'), (2, 'b'), (2, 'x')]
    partition (eq 4) l === []         * [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]

prop_list_predicate_break :: Property
prop_list_predicate_break = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (4, 'b'), (3, 'c'), (4, 'd')]
        break = Data.AssocList.List.Predicate.break
        eq x = Predicate (== x)
        (*) = (,)

    break (eq 1) l === [] * [(1, 'a'),    (4, 'b'),    (3, 'c'), (4, 'd')]
    break (eq 2) l === [     (1, 'a'),    (4, 'b'),    (3, 'c'), (4, 'd')] * []
    break (eq 3) l === [     (1, 'a'),    (4, 'b')] * [(3, 'c'), (4, 'd')]
    break (eq 4) l === [     (1, 'a')] * [(4, 'b'),    (3, 'c'), (4, 'd')]
