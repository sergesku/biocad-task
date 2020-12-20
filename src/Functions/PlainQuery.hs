{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Functions.PlainQuery where

import Types

import Control.Monad
import Data.Text hiding (zip, concat)
import Database.Bolt
import Database.Bolt.Extras


putReactionNode :: Reaction -> BoltActionT IO [Record]
putReactionNode Reaction{..} = queryP "MERGE (r:Reaction {name : {name}}) RETURN ID(r) AS id"
                                $ props ["name" =: getName r'name]

putMoleculeNode :: Molecule -> BoltActionT IO [Record]
putMoleculeNode Molecule{..} = queryP "MERGE (m:Molecule {smiles : {smiles}, iupacName : {iupacName}}) RETURN ID(m) AS id"
                                $ props ["smiles" =: getSmiles m'smiles, "iupacName" =: getName m'iupacName]

putCatalystNode :: Catalyst -> BoltActionT IO [Record]
putCatalystNode Catalyst{..} = queryP ( "MERGE (c:Catalyst {smiles : {smiles}" <> mbName <> "}) RETURN ID(c) AS id" )
                                $ props $ mbProp <> ["smiles" =: getSmiles c'smiles]
  where (mbName, mbProp) = case c'name of
                             Nothing -> ("", [])
                             Just x  -> (", name : {name}", ["name" =: getName x])

putReagentInRel :: Id Molecule -> Id Reaction -> BoltActionT IO [Record]
putReagentInRel idm idr = queryP ( "MATCH (m:Molecule),(r:Reaction) WHERE ID(m) = {idm} AND ID(r) = {idr}" 
                                 <> "MERGE (m)-[rel:REAGENT_IN]->(r) RETURN ID(rel) AS id" )
                                 $ props ["idm" =: getId idm, "idr" =: getId idr]

putProductFromRel :: PRODUCT_FROM -> Id Reaction -> Id Molecule -> BoltActionT IO [Record]
putProductFromRel PRODUCT_FROM{..} idr idm = queryP ( "MATCH (r:Reaction),(m:Molecule) WHERE ID(r) = {idr} AND ID(m) = {idm}"
                                                    <> "MERGE (r)-[rel:PRODUCT_FROM {amount : {amount}}]->(m) RETURN ID(rel) AS id" )
                                                  $ props ["idr" =: getId idr, "idm" =: getId idm, "amount" =: getAmount p'amount]

putAccelerateRel :: ACCELERATE -> Id Catalyst -> Id Reaction -> BoltActionT IO [Record]
putAccelerateRel ACCELERATE{..} idc idr =
  queryP ( "MATCH (c:Catalyst),(r:Reaction) WHERE ID(c) = {idc} AND ID(r) = {idr}"
         <> "MERGE (c)-[rel:ACCELERATE {temperature : {temperature}, pressure : {pressure}}]->(r) RETURN ID(rel) AS id" )
         $ props ["idc" =: getId idc, "idr" =: getId idr, "temperature" =: getTemp a'temperature, "pressure" =: getPressure a'pressure]



putReaction :: ReactionData -> BoltActionT IO ()
putReaction ReactionData{..} = do
  let (prodNs, prodRs) = unzip rdProducts
      (catNs, accelRs) = unzip rdCatalyst
  reactIdResp <- putReactionNode rdReaction
  reagIdResp  <- forM rdReagents putMoleculeNode
  prodIdResp  <- forM prodNs putMoleculeNode
  catIdResp   <- forM catNs putCatalystNode

  reactIds :: [Id Reaction] <- forM reactIdResp $ \rec -> Id <$> rec `at` "id"
  reagIds  :: [Id Molecule] <- forM (concat reagIdResp) $ \rec -> Id <$> rec `at` "id"
  prodIds  :: [Id Molecule] <- forM (concat prodIdResp) $ \rec -> Id <$> rec `at` "id"
  catIds   :: [Id Catalyst] <- forM (concat catIdResp)  $ \rec -> Id <$> rec `at` "id"

  case reactIds of
    []    -> return ()
    [idr] -> do forM_ reagIds $ \idm -> putReagentInRel idm idr
                forM_ (zip prodIds prodRs) $ \(idm, rel) -> putProductFromRel rel idr idm
                forM_ (zip catIds accelRs) $ \(idc, rel) -> putAccelerateRel  rel idc idr



getReactionNode :: Id Reaction -> BoltActionT IO (Maybe Reaction)
getReactionNode (Id i) = do 
  resp <- queryP "MATCH (r:Reaction) WHERE ID(r) = {i} RETURN r" $ props ["i" =: i]
  case resp of
    [rec] -> (Just . fromNode) <$> rec `at` "r"
    _     -> return Nothing