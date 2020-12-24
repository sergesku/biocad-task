module Main where


import Types
import SampleData

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Text
import Data.Text as T (concat)
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

createTestReactionQuery :: Text
createTestReactionQuery = T.concat [ "CREATE (r:Reaction {name: \"rName\"})"
                                   , "CREATE (m1: Molecule {smiles: \"m1Smiles\", iupacName: \"m1IupacName\"})"
                                   , "CREATE (m2: Molecule {smiles: \"m2Smiles\", iupacName: \"m2IupacName\"})"
                                   , "CREATE (m3: Molecule {smiles: \"m3Smiles\", iupacName: \"m3IupacName\"})"
                                   , "CREATE (c1: Catalyst {smiles: \"c1Smiles\", name: \"c1Name\"})"
                                   , "CREATE (c2: Catalyst {smiles: \"c2Smiles\"})"
                                   , "CREATE (m1)-[:REAGENT_IN]->(r)"
                                   , "CREATE (m2)-[:REAGENT_IN]->(r)"
                                   , "CREATE (r)-[:PRODUCT_FROM {amount: 12.4}]->(m3)"
                                   , "CREATE (c1)-[:ACCELERATE {temperature: 314.23, pressure: 132.99}]->(r)"
                                   , "CREATE (c2)-[:ACCELERATE {temperature: 4.60, pressure: 12.01}]->(r)"
                                   , "RETURN id(r) as id"
                                   ]
r :: Reaction
r = Reaction (Name "rName")

m1,m2,m3 :: Molecule
m1 = Molecule (Smiles "m1Smiles") (Name "m1IupacName")
m2 = Molecule (Smiles "m2Smiles") (Name "m2IupacName")
m3 = Molecule (Smiles "m3Smiles") (Name "m3IupacName")

c1, c2 :: Catalyst
c1 = Catalyst (Smiles "c1Smiles") (Just $ Name "c1Name")
c2 = Catalyst (Smiles "c2Smiles") Nothing

p1 :: PRODUCT_FROM
p1 = PRODUCT_FROM (Amount 12.4)

a1,a2 :: ACCELERATE
a1 = ACCELERATE (Temp 314.23) (Pressure 132.99)
a2 = ACCELERATE (Temp 4.6) (Pressure 12.01)

testReaction1 :: ReactionData
testReaction1 = ReactionData r [m1,m2] [(m3, p1)] [(c1, a1),(c2,a2)]

main :: IO ()
main = hspec $ do
  describe "Functions.TextQuery" $ do 
    describe "getReaction . putReaction" $
      it "gets initial reaction back" $ do
        reaction    <- randomReaction
        idr         <- runQueryDB $ putReaction reaction
        mbReaction  <- runQueryDB $ getReaction idr
        mbReaction `shouldBe` (pure reaction)

    describe "getReaction" $
      it "gets proper reaction with 2 reagents, 1 products, 2 catalyst from testDatabase" $ do
        mbReaction <- runQueryDB $ query createTestReactionQuery >>= unpackSingleId >>= getReaction
        mbReaction `shouldBe` (pure testReaction1)

 