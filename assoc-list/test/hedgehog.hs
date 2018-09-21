{-# LANGUAGE TemplateHaskell #-}

import           Data.AssocList.Exception
import           Data.AssocList.List.Type
import qualified Data.AssocList.List.Eq
import qualified Data.AssocList.List.Equivalence
import qualified Data.AssocList.List.Predicate

-- base
import GHC.Stack (HasCallStack)
import           Control.Exception (try, Exception)
import           Control.Monad  (unless)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Foldable  (for_)
import qualified System.Exit    as Exit
import qualified System.IO      as IO

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

prop_eq_bang :: Property
prop_eq_bang = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        (!) = (Data.AssocList.List.Eq.!)

    l ! 1 === 'a'
    l ! 2 === 'b'
    l ! 3 === 'c'
    throws (l ! 4) MissingAssocListKey

prop_eq_bang_maybe :: Property
prop_eq_bang_maybe = withTests 1 $ property $ do

    let
        l = [(1, 'a'), (2, 'b'), (2, 'x'), (3, 'c')]
        (!?) = (Data.AssocList.List.Eq.!?)

    l !? 1 === Just 'a'
    l !? 2 === Just 'b'
    l !? 3 === Just 'c'
    l !? 4 === Nothing
