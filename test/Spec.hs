module Main where

import Types
import SampleData
import Functions.Utils
import Functions.TextRequest

import Test.Hspec
import Data.Text
import Data.Text as T (concat)
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

createTestReactionQuery1 :: Text
createTestReactionQuery1 = T.concat [ "CREATE (r:Reaction {name: \"rName\"})"
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

testReaction2 :: ReactionData
testReaction2 = ReactionData r [] [] []

createTestReactionQuery2 :: Text
createTestReactionQuery2 = "CREATE (r:Reaction {name: \"rName\"}) RETURN id(r) as id"


main :: IO ()
main = hspec $ do
  describe "Functions.TextQuery" $ do
    describe "getReaction" $ do
        it "gets proper reaction with 2 reagents, 1 products, 2 catalyst from Database" $ do
            mbReaction <- runQueryDB $ query createTestReactionQuery1 >>= unpackSingleId >>= getReaction
            mbReaction `shouldBe` pure testReaction1
  
        it "gets proper reaction with 0 reagents, 0 products, 0 catalyst from the Database" $ do
            mbReaction <- runQueryDB $ query createTestReactionQuery2 >>= unpackSingleId >>= getReaction
            mbReaction `shouldBe` pure testReaction2
  
        it "gets Nothing when reaction doesn`t exist in the Database" $ do
            reaction   <- randomReaction
            mbReaction <- runQueryDB $ putReaction reaction >>= (\i -> deleteReaction i >> getReaction i)
            mbReaction `shouldBe` Nothing
      
        it "doesn`t change reaction in the Database" $ do
            reaction <- randomReaction
            idr      <- runQueryDB $ putReaction reaction 
            mbReaction1 <- runQueryDB $ getReaction idr
            mbReaction2 <- runQueryDB $ getReaction idr
            mbReaction1 `shouldBe` mbReaction2

    describe "putReaction + getReaction" $
      it "gets initial reaction back" $ do
          reaction   <- randomReaction
          mbReaction <- runQueryDB $ putReaction reaction >>= getReaction
          mbReaction `shouldBe` pure reaction
      
    describe "putReaction" $
      it "don`t allow to modify the existing reaction" $ do
        mbReaction <- runQueryDB $ do putReaction testReaction2
                                      (putReaction testReaction1) >>= getReaction
        mbReaction `shouldBe` pure testReaction2