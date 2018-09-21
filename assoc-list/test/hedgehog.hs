{-# LANGUAGE TemplateHaskell #-}

import           Data.AssocList.Exception
import           Data.AssocList.List.Type
import qualified Data.AssocList.List.Eq
import qualified Data.AssocList.List.Equivalence
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
import Data.Functor.Contravariant (Equivalence (..), Predicate (..),
                                   defaultEquivalence)

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


--------------------------------------------------------------------------------
--  Data.AssocList.List.Equivalence
--------------------------------------------------------------------------------

prop_list_equivalence_lookupFirst :: Property
prop_list_equivalence_lookupFirst = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        lookupFirst = Data.AssocList.List.Equivalence.lookupFirst

    lookupFirst defaultEquivalence 1 l === Just 'a'
    lookupFirst defaultEquivalence 2 l === Just 'b'
    lookupFirst defaultEquivalence 3 l === Just 'c'
    lookupFirst defaultEquivalence 4 l === Nothing

prop_list_equivalence_lookupAll :: Property
prop_list_equivalence_lookupAll = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        lookupAll = Data.AssocList.List.Equivalence.lookupAll

    lookupAll defaultEquivalence 1 l === ['a']
    lookupAll defaultEquivalence 2 l === ['b', 'x']
    lookupAll defaultEquivalence 3 l === ['c']
    lookupAll defaultEquivalence 4 l === []

prop_list_equivalence_removeFirst :: Property
prop_list_equivalence_removeFirst = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        removeFirst = Data.AssocList.List.Equivalence.removeFirst

    removeFirst defaultEquivalence 1 l === [(2, 'b'), (2, 'x'), (3, 'c')]
    removeFirst defaultEquivalence 2 l === [(1, 'a'), (2, 'x'), (3, 'c')]
    removeFirst defaultEquivalence 3 l === [(1, 'a'), (2, 'b'), (2, 'x')]
    removeFirst defaultEquivalence 4 l === [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]

prop_list_equivalence_removeAll :: Property
prop_list_equivalence_removeAll = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        removeAll = Data.AssocList.List.Equivalence.removeAll

    removeAll defaultEquivalence 1 l === [(2, 'b'), (2, 'x'), (3, 'c')]
    removeAll defaultEquivalence 2 l === [(1, 'a'), (3, 'c')]
    removeAll defaultEquivalence 3 l === [(1, 'a'), (2, 'b'), (2, 'x')]
    removeAll defaultEquivalence 4 l === [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
