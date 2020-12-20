{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Functions.PlainQuery where

import Types

import Control.Monad
import Data.Text
import Database.Bolt


putReactionNode :: Reaction -> BoltActionT IO [Record]
putReactionNode Reaction{..} = queryP "MERGE (:Reaction {name : {r'name}}) RETURN ID({n}) AS id"
                                $ props ["r'name" =: getName r'name]

putMoleculeNode :: Molecule -> BoltActionT IO [Record]
putMoleculeNode Molecule{..} = queryP "MERGE (:Molecule {smiles : {m'smiles}, iupacName : {m'iupacName}})"
                                $ props ["m'smiles" =: getSmiles m'smiles, "m'iupacName" =: getName m'iupacName]

putCatalystNode :: Catalyst -> BoltActionT IO [Record]
putCatalystNode Catalyst{..} = queryP ( "MERGE (:Catalyst {smiles : {c'smiles}" <> mbName <> "})" )
                                $ props $ mbProp <> ["m'smiles" =: getSmiles c'smiles]
  where (mbName, mbProp) = case c'name of
                             Nothing -> ("", [])
                             Just x  -> (", name : {c'name}", ["c'name" =: getName x])

putReagentInRel :: Id Molecule -> Id Reaction -> BoltActionT IO [Record]
putReagentInRel idm idr = queryP ( "MATCH (m:Molecule),(r:Reaction) WHERE ID(m) = {idm} AND ID(r) = {idr}" 
                                 <> "MERGE (m)-[rel:REAGENT_IN]->(r) RETURN ID(rel) AS id" )
                                 $ props ["idm" =: getId idm, "idr" =: getId idr]

putProductFromRel :: PRODUCT_FROM -> Id Molecule -> Id Reaction -> BoltActionT IO [Record]
putProductFromRel PRODUCT_FROM{..} idr idm = queryP ( "MATCH (r:Reaction),(m:Molecule) WHERE ID(r) = {idr} AND ID(m) = {idm}"
                                                    <> "MERGE (r)-[rel:PRODUCT_FROM {amount : {p'amount}}]->(m) RETURN ID(rel) AS id" )
                                                  $ props ["idr" =: getId idr, "idm" =: getId idm, "p'amount" =: getAmount p'amount]

putAccelerateRel :: ACCELERATE -> Id Catalyst -> Id Reaction -> BoltActionT IO [Record]
putAccelerateRel ACCELERATE{..} idc idr = queryP ( "MATCH (c:Catalyst),(r:Reaction) WHERE ID(c) = {idc} AND ID(r) = {idr}"
                                                 <> "MERGE (c)-[rel:ACCELERATE {temperature : {a'temperature}, pressure : {a'pressure}}]->(r) RETURN ID(rel) AS id" )
                                                 $ props ["idc" =: getId idc, "idr" =: getId idr, "a'temperature" =: getTemp a'temperature, "a'pressure" =: getPressure a'pressure]