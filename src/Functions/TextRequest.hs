module Functions.TextRequest
  ( putReaction
  , getReaction
  , findShortPath
  , findShortPathById
  , deleteReaction
  ) where

import Types
import Functions.TextRequest.Internal

import Control.Monad                (forM, forM_)
import Control.Monad.IO.Class       (liftIO)
import qualified Data.Text as T     (concat)
import Database.Bolt


putReaction :: ReactionData -> BoltActionT IO (Id Reaction)
putReaction ReactionData{..} = transact $ do
  let (prodNs, prodRs) = unzip rdProducts
      (catNs, accelRs) = unzip rdCatalyst
  lst <- matchReactionNameNode rdReaction
  case lst of
    (x:_) -> do liftIO $ print $ "Warning | Reaction " ++ (show . getName . r'name $ rdReaction) ++ " already exists. Id: " ++ (show . getId $ x)
                return x
    []    -> do reactId <- putReactionNode rdReaction
                reagIds <- forM rdReagents putMoleculeNode
                prodIds <- forM prodNs putMoleculeNode
                catIds  <- forM catNs putCatalystNode
                forM_ reagIds $ \idm -> putReagentInRel idm reactId
                forM_ (zip prodIds prodRs) $ \(molId, rel) -> putProductFromRel rel reactId molId
                forM_ (zip catIds accelRs) $ \(catId, rel) -> putAccelerateRel  rel catId reactId
                pure reactId


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


findShortPath :: Molecule -> Molecule -> BoltActionT IO [Transformation]
findShortPath start end = queryP queryText properties >>= mapM (`at` "pathNodes")
  where
    queryText = T.concat [ "MATCH (start:Molecule {smiles : {smiles1}, iupacName : {iupacName1}})"
                         , "MATCH (end:Molecule {smiles : {smiles2}, iupacName : {iupacName2}})"
                         , "MATCH path=allShortestPaths((start)-[:REAGENT_IN | :PRODUCT_FROM *]->(end))"
                         , "WHERE ALL(n in nodes(path) WHERE n:Molecule OR n:Reaction)"
                         , "RETURN nodes(path) AS pathNodes"
                         ]
    properties = props [ "smiles1" =: (getSmiles . m'smiles $ start)
                       , "smiles2" =: (getSmiles . m'smiles $ end)
                       , "iupacName1" =: (getName . m'iupacName $ start)
                       , "iupacName2" =: (getName . m'iupacName $ end)
                       ]


findShortPathById :: Id Molecule -> Id Molecule -> BoltActionT IO [Transformation]
findShortPathById startId endId = queryP queryText properties >>= mapM (`at` "pathNodes")
  where 
    properties = props [ "startId" =: getId startId, "endId" =: getId endId]
    queryText  = T.concat [ "MATCH (start:Molecule) WHERE id(start) = {startId}"
                          , "MATCH (end:Molecule) WHERE id(end) = {endId}"
                          , "MATCH path=allShortestPaths((start)-[:REAGENT_IN | :PRODUCT_FROM *]->(end))"
                          , "WHERE ALL(n in nodes(path) WHERE n:Molecule OR n:Reaction)"
                          , "RETURN nodes(path) AS pathNodes"
                          ]

deleteReaction :: Id Reaction -> BoltActionT IO ()
deleteReaction i = queryP_ "MATCH p=(r:Reaction)-[rel]-() WHERE id(r) = {idr} DELETE rel,r" $ props ["idr" =: getId i]