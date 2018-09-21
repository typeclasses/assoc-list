module Data.AssocList.Exception where

-- base
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Prelude (Eq, Show)

data MissingAssocListKey = MissingAssocListKey
  deriving (Eq, Show, Typeable)

instance Exception MissingAssocListKey
