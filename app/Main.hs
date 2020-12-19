{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types
import Data.Text
import Functions
import Control.Exception
import Data.Default
import Database.Bolt
import Database.Bolt.Extras.Graph

r = Reaction (Name "ReactName")
m1 = Molecule (Smiles "HOH") (Name "Mol-1-Name")
m2 = Molecule (Smiles "C-IU") (Name "Mol-2-Name")
m3 = Molecule (Smiles "H") (Name "Mol-3-Name")
m4 = Molecule (Smiles "GHDS") (Name "Mol-4-Name")

rd = ReactionData r [m1,m2,m3] [(m4, PRODUCT_FROM (Amount 14))] []

boltCfg :: BoltCfg
boltCfg = def { host = "localhost"
              , user = "neo4j"
              , password = "movie"
              }

runQueryDB :: BoltActionT IO a -> IO a
runQueryDB act = bracket (connect boltCfg) close (`run` act)

main :: IO ()
main = do
  resp <- runQueryDB $ putReactionGraph rd
  print resp
  
