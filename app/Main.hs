module Main where

import Types
import SampleData
import Data.Text
import Functions.TextRequest
import Data.Default
import Database.Bolt
import Database.Bolt.Extras.Graph
import Control.Monad (replicateM, forM, forM_)
import Control.Exception (bracket)


boltCfg :: BoltCfg
boltCfg = def { host = "localhost"
              , user = "neo4j"
              , password = "testDB"
              }


runQueryDB :: BoltActionT IO a -> IO a
runQueryDB act = bracket (connect boltCfg) close (`run` act)


main :: IO ()
main = do
  reacts <- replicateM 50 randomReaction
  ids <- forM reacts $ runQueryDB . putReaction
  forM_ ids $ \ (Id i) -> print $ "Created reaction: Id " ++ show i
