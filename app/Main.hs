module Main where

import Types
import SampleData                             (putSampleData)
import Functions.TextRequest
import qualified Functions.GraphRequest as G  

import Database.Bolt
import Data.Default                           (def)
import Control.Exception                      (bracket)


boltCfg :: BoltCfg
boltCfg = def { host = "localhost"
              , user = "neo4j"
              , password = "testDB"
              }

runQueryDB :: BoltActionT IO a -> IO a
runQueryDB act = bracket (connect boltCfg) close (`run` act)

main :: IO ()
main = do
  putStrLn "Enter number of generated reactions:"
  n <- read <$> getLine
  runQueryDB $ putSampleData n
