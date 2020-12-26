module Main where

import Types
import SampleData
import Functions.Utils
import qualified Functions.TextRequest           as TR
import qualified Functions.TextRequest.Internal  as ITR
import qualified Functions.GraphRequest          as GR

import Test.Hspec
import Data.Text
import Data.Text as T     (concat)
import Data.Default       (def)
import Database.Bolt
import Control.Monad      (void)
import Control.Exception  (bracket)

boltCfg :: BoltCfg
boltCfg = def { host = "localhost"
              , user = "neo4j"
              , password = "testDB"
              }

runQueryDB :: BoltActionT IO a -> IO a
runQueryDB act = bracket (connect boltCfg) close (`run` act)

flushDB :: BoltActionT IO ()
flushDB = query_ "MATCH (node) DETACH DELETE node"

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
        it "gets proper Reaction with 2 reagents, 1 products, 2 catalyst from the Database" $ do
            mbReaction <- runQueryDB $ do flushDB
                                          idr <- query createTestReactionQuery1 >>= unpackSingleId
                                          TR.getReaction idr
            mbReaction `shouldBe` pure testReaction1
  
        it "gets proper Reaction with 0 reagents, 0 products, 0 catalyst from the Database" $ do
            mbReaction <- runQueryDB $ do flushDB
                                          idr <- query createTestReactionQuery2 >>= unpackSingleId
                                          TR.getReaction idr
            mbReaction `shouldBe` pure testReaction2
  
        it "gets Nothing when Reaction doesn`t exist in the Database" $ do
            reaction   <- randomReactionData
            mbReaction <- runQueryDB $ do flushDB
                                          idr <- TR.putReaction reaction
                                          TR.deleteReaction idr
                                          TR.getReaction idr
            mbReaction `shouldBe` Nothing
      
        it "doesn`t change Reaction in the Database" $ do
            reaction    <- randomReactionData
            mbReaction2 <- runQueryDB $ do flushDB
                                           idr <- TR.putReaction reaction 
                                           void $ TR.getReaction idr
                                           TR.getReaction idr
            mbReaction2 `shouldBe` pure reaction

    describe "putReaction" $
      it "doesn`t allow to modify an existing Reaction" $ do
        mbReaction <- runQueryDB $ do flushDB
                                      void $ TR.putReaction testReaction2
                                      idr <- TR.putReaction testReaction1
                                      TR.getReaction idr
        mbReaction `shouldBe` pure testReaction2
    
    describe "putReaction + getReaction" $
      it "gets initial Reaction back" $ do
          reaction   <- randomReactionData
          mbReaction <- runQueryDB $ flushDB >> TR.putReaction reaction >>= TR.getReaction
          mbReaction `shouldBe` pure reaction

    describe "findShortPathById" $ 
      context "when start and end are the same" $ do
        context "when Molecule exists in the Database" $ 
          it "gets one Transformation in which only this single Molecule" $ do
              molecule   <- randomMolecule
              (idm, lst) <- runQueryDB $ do flushDB
                                            idm <- ITR.putMoleculeNode molecule
                                            lst <- TR.findShortPathById idm idm
                                            pure (idm, lst)
              lst `shouldBe` [[MoleculeNode idm molecule]]

        context "when Molecule doesn`t exist in the Database" $
          it "gets empty list of Transformation" $ do
              lst <- runQueryDB $ flushDB >> TR.findShortPathById (Id 145) (Id 145)
              lst `shouldBe` [[]]

    describe "findShortPath" $ 
      context "when start and end are the same" $ do
        context "when Molecule exists in the Database" $ 
          it "gets one Transformation in which only this single Molecule" $ do
              molecule   <- randomMolecule
              (idm, lst) <- runQueryDB $ do flushDB
                                            idm <- ITR.putMoleculeNode molecule
                                            lst <- TR.findShortPath molecule molecule
                                            pure (idm, lst)
              lst `shouldBe` [[MoleculeNode idm molecule]]

        context "when Molecule doesn`t exist in the Database" $
          it "gets empty list of Transformation" $ do
              lst <- runQueryDB $ flushDB >> TR.findShortPath m1 m1
              lst `shouldBe` [[]]

  describe "Functions.GraphQuery" $ do
    describe "getReaction" $ do
        it "gets proper Reaction with 2 reagents, 1 products, 2 catalyst from the Database" $ do
            mbReaction <- runQueryDB $ do flushDB
                                          idr <- query createTestReactionQuery1 >>= unpackSingleId
                                          GR.getReaction idr
            mbReaction `shouldBe` pure testReaction1
  
        it "gets proper Reaction with 0 reagents, 0 products, 0 catalyst from the Database" $ do
            mbReaction <- runQueryDB $ do flushDB
                                          idr <- query createTestReactionQuery2 >>= unpackSingleId
                                          GR.getReaction idr
            mbReaction `shouldBe` pure testReaction2
  
        it "gets Nothing when Reaction doesn`t exist in the Database" $ do
            reaction   <- randomReactionData
            mbReaction <- runQueryDB $ do flushDB
                                          idr <- GR.putReaction reaction
                                          TR.deleteReaction idr
                                          GR.getReaction idr
            mbReaction `shouldBe` Nothing
      
        it "doesn`t change Reaction in the Database" $ do
            reaction    <- randomReactionData
            mbReaction2 <- runQueryDB $ do flushDB
                                           idr <- GR.putReaction reaction 
                                           void $ GR.getReaction idr
                                           GR.getReaction idr
            mbReaction2 `shouldBe` pure reaction

    describe "putReaction" $
      it "doesn`t allow to modify an existing Reaction" $ do
        mbReaction <- runQueryDB $ do flushDB
                                      void $ GR.putReaction testReaction2
                                      idr <- GR.putReaction testReaction1
                                      GR.getReaction idr
        mbReaction `shouldBe` pure testReaction2
    
    describe "putReaction + getReaction" $
      it "gets initial Reaction back" $ do
          reaction   <- randomReactionData
          mbReaction <- runQueryDB $ flushDB >> GR.putReaction reaction >>= GR.getReaction
          mbReaction `shouldBe` pure reaction
            