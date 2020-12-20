{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types
import Data.Text
import Functions.Graph
import Control.Exception
import Data.Default
import Database.Bolt
import Database.Bolt.Extras.Graph

r = Reaction (Name "Rxx")
m1 = Molecule (Smiles "HwqeOH-2") (Name "Mol-x-Name")
m2 = Molecule (Smiles "C123-IU-2") (Name "Mol-y-Name")
m3 = Molecule (Smiles "H-312") (Name "Mol-z-Name")
m4 = Molecule (Smiles "GH425DS-2") (Name "Mol-q-Name")

rd = ReactionData r [m1,m2,m3] [(m4, PRODUCT_FROM (Amount 2.1))] []

boltCfg :: BoltCfg
boltCfg = def { host = "localhost"
              , user = "neo4j"
              , password = "movie"
              }

runQueryDB :: BoltActionT IO a -> IO a
runQueryDB act = bracket (connect boltCfg) close (`run` act)

main :: IO ()
main = runQueryDB $ putReaction rd
