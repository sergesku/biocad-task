module Main where

import SampleData                 (randomReaction)
import Functions.TextRequest

import Data.Default               (def)
import Database.Bolt
import Control.Exception          (bracket)
import Control.Monad              (replicateM, forM_)
import Control.Monad.IO.Class     (liftIO)

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
  runQueryDB $ forM_ reacts $ \r -> do i <- putReaction r
                                       liftIO $ print $ "Created reaction: Id " ++ show i