module Main where

import Types
import SampleData                 (randomReaction)
import Functions.TextRequest

import Data.Default               (def)
import Database.Bolt
import Control.Exception          (bracket)
import Control.Monad              (replicateM, forM, forM_)

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
