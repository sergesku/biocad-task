{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Functions.PlainQuery where

import Types

import Data.List (foldl')
import Control.Applicative (liftA2)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Data.Text (Text)
import qualified Data.Text as T
import Database.Bolt
import Database.Bolt.Extras
import Database.Bolt.Serialization

unpackSingleId :: [Record] -> BoltActionT IO (Id a)
unpackSingleId (rec:_) = Id <$> rec `at` "id"
unpackSingleId [] = throwError $ NoStructureInResponse

putReactionNode :: Reaction -> BoltActionT IO (Id Reaction)
putReactionNode Reaction{..} = resp >>= unpackSingleId
  where resp = queryP "MERGE (r:Reaction {name : {name}}) RETURN ID(r) AS id"
                      $ props ["name" =: getName r'name]

putMoleculeNode :: Molecule -> BoltActionT IO (Id Molecule)
putMoleculeNode Molecule{..} = resp >>= unpackSingleId
  where resp = queryP "MERGE (m:Molecule {smiles : {smiles}, iupacName : {iupacName}}) RETURN ID(m) AS id"
                      $ props ["smiles" =: getSmiles m'smiles, "iupacName" =: getName m'iupacName]

putCatalystNode :: Catalyst -> BoltActionT IO (Id Catalyst)
putCatalystNode Catalyst{..} = resp >>= unpackSingleId
  where resp = queryP ( "MERGE (c:Catalyst {smiles : {smiles}" <> mbName <> "}) RETURN ID(c) AS id" )
                      $ props $ mbProp <> ["smiles" =: getSmiles c'smiles]
        (mbName, mbProp) = case c'name of
                            Nothing -> ("", [])
                            Just x  -> (", name : {name}", ["name" =: getName x])

putReagentInRel :: Id Molecule -> Id Reaction -> BoltActionT IO (Id REAGENT_IN)
putReagentInRel idm idr = resp >>= unpackSingleId
  where resp = queryP ( "MATCH (m:Molecule),(r:Reaction) WHERE ID(m) = {idm} AND ID(r) = {idr}" 
                      <> "MERGE (m)-[rel:REAGENT_IN]->(r) RETURN ID(rel) AS id" )
                      $ props ["idm" =: getId idm, "idr" =: getId idr]
  
putProductFromRel :: PRODUCT_FROM -> Id Reaction -> Id Molecule -> BoltActionT IO (Id PRODUCT_FROM)
putProductFromRel PRODUCT_FROM{..} idr idm = resp >>= unpackSingleId
  where resp = queryP ( "MATCH (r:Reaction),(m:Molecule) WHERE ID(r) = {idr} AND ID(m) = {idm}"
                      <> "MERGE (r)-[rel:PRODUCT_FROM {amount : {amount}}]->(m) RETURN ID(rel) AS id" )
                      $ props ["idr" =: getId idr, "idm" =: getId idm, "amount" =: getAmount p'amount]

putAccelerateRel :: ACCELERATE -> Id Catalyst -> Id Reaction -> BoltActionT IO (Id ACCELERATE)
putAccelerateRel ACCELERATE{..} idc idr = resp >>= unpackSingleId
  where resp = queryP ( "MATCH (c:Catalyst),(r:Reaction) WHERE ID(c) = {idc} AND ID(r) = {idr}"
                      <> "MERGE (c)-[rel:ACCELERATE {temperature : {temperature}, pressure : {pressure}}]->(r) RETURN ID(rel) AS id" )
                      $ props ["idc" =: getId idc, "idr" =: getId idr, "temperature" =: getTemp a'temperature, "pressure" =: getPressure a'pressure]

putReaction :: ReactionData -> BoltActionT IO (Id Reaction)
putReaction ReactionData{..} = transact $ do
  let (prodNs, prodRs) = unzip rdProducts
      (catNs, accelRs) = unzip rdCatalyst
  reactId <- putReactionNode rdReaction
  reagIds <- forM rdReagents putMoleculeNode
  prodIds <- forM prodNs putMoleculeNode
  catIds  <- forM catNs putCatalystNode
  forM_ reagIds $ \idm -> putReagentInRel idm reactId
  forM_ (zip prodIds prodRs) $ \(molId, rel) -> putProductFromRel rel reactId molId
  forM_ (zip catIds accelRs) $ \(catId, rel) -> putAccelerateRel  rel catId reactId
  pure reactId



getReactionNode :: Id Reaction -> BoltActionT IO (Maybe Reaction)
getReactionNode (Id i) = do 
  resp <- queryP "MATCH (r:Reaction) WHERE ID(r) = {i} RETURN r AS reaction" $ props ["i" =: i]
  case resp of
    [rec] -> (Just . fromNode) <$> rec `at` "reaction"
    _     -> pure Nothing

getReagentNodeRel :: Id Reaction -> BoltActionT IO [(Molecule,REAGENT_IN)]
getReagentNodeRel (Id i) = do
  resp <- queryP "MATCH (m:Molecule)-[rel:REAGENT_IN]->(r:Reaction) WHERE ID(r) = {i} RETURN m AS molecule, rel as reagentIn" $ props ["i" =: i]
  forM resp $ \rec -> liftA2 (,) (fromNode <$> rec `at` "molecule") (fromRelation <$> rec `at` "reagentIn")
  
getProductNodeRel :: Id Reaction -> BoltActionT IO [(Molecule, PRODUCT_FROM)]
getProductNodeRel (Id i) = do
  resp <- queryP "MATCH (r:Reaction)-[rel:PRODUCT_FROM]->(m:Molecule) WHERE ID(r) = {i} RETURN m AS molecule, rel AS productFrom" $ props ["i" =: i]
  forM resp $ \rec -> liftA2 (,) (fromNode <$> rec `at` "molecule") (fromRelation <$> rec `at` "productFrom")

getCatalystNodeRel :: Id Reaction -> BoltActionT IO [(Catalyst, ACCELERATE)]
getCatalystNodeRel (Id i) = do
  resp <- queryP "MATCH (c:Catalyst)-[rel:ACCELERATE]->(r:Reaction) WHERE ID(r) = {i} RETURN c AS catalyst, rel AS accelerate" $ props ["i" =: i]
  forM resp  $ \rec -> liftA2 (,) (fromNode <$> rec `at` "catalyst") (fromRelation <$> rec `at` "accelerate")


fromRelation :: URelationLike a => Relationship -> a
fromRelation = fromURelation . convertRelType

convertRelType :: Relationship -> URelationship
convertRelType Relationship{..} = URelationship relIdentity relType relProps

getReaction :: Id Reaction -> BoltActionT IO (Maybe ReactionData)
getReaction i = do
  reaction   <- getReactionNode i
  reagents   <- getReagentNodeRel i
  rdProducts <- getProductNodeRel i
  rdCatalyst <- getCatalystNodeRel i
  let rdReagents = fmap fst reagents
  case reaction of
    Nothing -> pure Nothing
    Just r  -> pure $ Just $ ReactionData r rdReagents rdProducts rdCatalyst

findShortPath :: Molecule -> Molecule -> BoltActionT IO [PathNode]
findShortPath m1 m2 = do
  let queryText = T.concat [ "MATCH p=(m1:Molecule {smiles : {smiles1}, iupacName : {iupacName1}})-[*]-(m2:Molecule {smiles : {smiles2}, iupacName : {iupacName2}})"
                           , "RETURN p ORDER BY length(p) DESC LIMIT 1"
                           ]
      properties = props [ "smiles1" =: (getSmiles . m'smiles $ m1)
                         , "smiles2" =: (getSmiles . m'smiles $ m2)
                         , "iupacName1" =: (getName . m'iupacName $ m1)
                         , "iupacName2" =: (getName . m'iupacName $ m2)
                         ]
  resp <- queryP queryText properties
  case resp of
    []  -> return []
    [rec] -> do segments <- rec `at` "segments"
                firstN :: Node   <- (head segments) `at` "start"
                restNs :: [Node] <- forM segments $ \rec -> rec `at` "end"
                let indexed = zip (firstN:restNs) [1..]
                    convert (node,i) | i `mod` 2 == 1 = MoleculeNode . fromNode $ node
                                     | otherwise      = ReactionNode . fromNode $ node
                return $ fmap convert indexed
                                     