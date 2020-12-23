{-# LANGUAGE ScopedTypeVariables #-}

module Functions.Utils 
  ( indexedNames
  , unpackSingleId
  ) where

import Types

import Database.Bolt
import Database.Bolt.Extras
import Data.Text                  (Text, pack)
import Control.Monad.Error.Class  (throwError)

indexedNames :: Text -> [Text]
indexedNames n = map ((n <>) . pack . show) [1..]

unpackSingleId :: [Record] -> BoltActionT IO (Id a)
unpackSingleId (rec:_) = Id <$> rec `at` "id"
unpackSingleId [] = throwError NoStructureInResponse