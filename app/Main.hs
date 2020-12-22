{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types
import Data.Text
import Functions.TextRequest
import Control.Exception
import Data.Default
import Database.Bolt
import Database.Bolt.Extras.Graph

r = Reaction (Name "PlainReactio")
m1 = Molecule (Smiles "Plain-HwqeOH-2") (Name "Plain-Mol-x-Name")
m2 = Molecule (Smiles "Plain-C123-IU-2") (Name "Plain-Mol-y-Name")
m3 = Molecule (Smiles "Plain-H-312") (Name "Plain-Mol-z-Name")
m4 = Molecule (Smiles "Plain-GH425DS-2") (Name "Plain-Mol-q-Name")
m5 = Molecule (Smiles "Plain-we25DS-2") (Name "Plain-Mwqerq-Name")
c1 = Catalyst (Smiles "Cat-1") Nothing
c2 = Catalyst (Smiles "Cat-1") (Just $ Name "Plain-Cat-2-Name")

r' = Reaction (Name "Reaction-2")
m1' = Molecule (Smiles "HwqeOH-2") (Name "ol-x-Name")
m2' = Molecule (Smiles "23-IU-2") (Name "Pl-y-Name")
m3' = Molecule (Smiles "312") (Name "Plal-z-Name")
m4' = Molecule (Smiles "5DS-2") (Name "Plain-ame")
m5' = Molecule (Smiles "e25DS-2") (Name "Plain-Mwame")
c1' = Catalyst (Smiles "Cat-11") Nothing
c2' = Catalyst (Smiles "Cat-12") (Just $ Name "Plainme")

rd = ReactionData r [m1,m2,m3] [(m4, PRODUCT_FROM (Amount 9.3)), (m5, PRODUCT_FROM (Amount 2.91))] [(c1, ACCELERATE (Temp 14.2) (Pressure 149.2)), (c2, ACCELERATE (Temp 149.3) (Pressure 143)) ]
rd' = ReactionData r' [m1',m2',m3'] [(m4', PRODUCT_FROM (Amount 4.3)), (m5', PRODUCT_FROM (Amount 12.31))] [(c1', ACCELERATE (Temp 12.5) (Pressure 49.22)), (c2', ACCELERATE (Temp 49.3) (Pressure 13)) ]

boltCfg :: BoltCfg
boltCfg = def { host = "localhost"
              , user = "neo4j"
              , password = "movie"
              }

runQueryDB :: BoltActionT IO a -> IO a
runQueryDB act = bracket (connect boltCfg) close (`run` act)

main :: IO ()
main = (runQueryDB $ putReaction rd') >>= print
