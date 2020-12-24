module Functions.Utils where

import Types
import Database.Bolt
import Control.Monad.Error.Class  (throwError)

unpackSingleId :: [Record] -> BoltActionT IO (Id a)
unpackSingleId (rec:_) = Id <$> rec `at` "id"
unpackSingleId [] = throwError NoStructureInResponse