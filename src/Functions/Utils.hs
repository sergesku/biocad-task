module Functions.Utils where

import Types
import Database.Bolt
import Control.Monad.Error.Class  (throwError)

unpackSingleId :: RecordValue a => [Record] -> BoltActionT IO (Id a)
unpackSingleId (rec:_) = rec `at` "id"
unpackSingleId [] = throwError NoStructureInResponse