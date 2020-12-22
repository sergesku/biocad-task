{-# LANGUAGE OverloadedStrings #-}

module Functions.Utils where

import Types

import Data.Text      (Text, pack)
import Database.Bolt
import Control.Monad.Error.Class

indexedNames :: Text -> [Text]
indexedNames n = map ((n <>) . pack . show) [1..]

unpackSingleId :: [Record] -> BoltActionT IO (Id a)
unpackSingleId (rec:_) = Id <$> rec `at` "id"
unpackSingleId [] = throwError $ NoStructureInResponse