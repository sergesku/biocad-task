module Functions.TextRequest.Internal where

import Types
import Functions.Utils

import Control.Applicative          (liftA2)
import Control.Monad                (forM)
import Database.Bolt


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


matchReactionNameNode :: Reaction -> BoltActionT IO [Id Reaction]
matchReactionNameNode Reaction{..} = do
  resp <- queryP "MATCH (r:Reaction {name : {name}}) RETURN id(r) AS idr" $ props ["name" =: getName r'name]
  forM resp $ \ rec -> rec `at` "idr"


getNode :: RecordValue a => Id a -> BoltActionT IO (Maybe a)
getNode (Id i) = do 
  resp <- queryP "MATCH (n) WHERE ID(n) = {i} RETURN n AS node" $ props ["i" =: i]
  case resp of
    [rec] -> rec `at` "node"
    _     -> pure Nothing


getReagentNodeRel :: Id Reaction -> BoltActionT IO [(Molecule,REAGENT_IN)]
getReagentNodeRel (Id i) = do
  resp <- queryP "MATCH (m:Molecule)-[rel:REAGENT_IN]->(r:Reaction) WHERE ID(r) = {i} RETURN m AS molecule, rel as reagentIn" $ props ["i" =: i]
  forM resp $ \rec -> liftA2 (,) (rec `at` "molecule") (rec `at` "reagentIn")
  

getProductNodeRel :: Id Reaction -> BoltActionT IO [(Molecule, PRODUCT_FROM)]
getProductNodeRel (Id i) = do
  resp <- queryP "MATCH (r:Reaction)-[rel:PRODUCT_FROM]->(m:Molecule) WHERE ID(r) = {i} RETURN m AS molecule, rel AS productFrom" $ props ["i" =: i]
  forM resp $ \rec -> liftA2 (,) (rec `at` "molecule") (rec `at` "productFrom")


getCatalystNodeRel :: Id Reaction -> BoltActionT IO [(Catalyst, ACCELERATE)]
getCatalystNodeRel (Id i) = do
  resp <- queryP "MATCH (c:Catalyst)-[rel:ACCELERATE]->(r:Reaction) WHERE ID(r) = {i} RETURN c AS catalyst, rel AS accelerate" $ props ["i" =: i]
  forM resp  $ \rec -> liftA2 (,) (rec `at` "catalyst") (rec `at` "accelerate")