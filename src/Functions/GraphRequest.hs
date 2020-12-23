{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Functions.GraphRequest
  ( putReaction
  , getReaction
  ) where

import Types
import Functions.Utils              (indexedNames)

import Control.Monad                (forM)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Error.Class    (throwError)
import Data.List                    (foldl')
import Data.Text                    (Text, pack)
import Data.Function                ((&))
import Database.Bolt
import Database.Bolt.Extras
import Database.Bolt.Extras.Graph
import qualified Data.Map as M      (lookup) 


putReaction :: ReactionData -> BoltActionT IO (Id Reaction)
putReaction rd@ReactionData{..} = do 
  reactGraph <- makeRequest @GetRequest [] (matchReactionNameGraph rdReaction)
  let rs :: [Reaction] = extractNode "reaction" <$> reactGraph
      idr = extractNodeId "reaction" <$> reactGraph
  case rs of
    (x:xs) -> do liftIO $ print $ "Warning | Reaction " ++ (show . getName . r'name $ rdReaction) ++ " already exists. Id: " ++ (show idr)
                 pure . Id . head $ idr
    []     -> do graphs <- makeRequest @PutRequest [] (putReactionGraph rd)
                 let mbI = M.lookup "reaction" (_vertices (head graphs))
                 case mbI of
                   Just i  -> pure $ Id i
                   Nothing -> throwError NoStructureInResponse


putReactionGraph :: ReactionData -> GraphPutRequest
putReactionGraph ReactionData{..} = foldl' (&) emptyGraph $ concat [reactionNode, reagentList, productList, catalystList]
  where
    reactionNode = [addNodeLike "reaction" rdReaction]
    reagentList  = nodeRelList ToReaction   "reagent"  $ zip rdReagents (repeat REAGENT_IN)
    productList  = nodeRelList FromReaction "product"  rdProducts
    catalystList = nodeRelList ToReaction   "catalyst" rdCatalyst


addNodeLike :: NodeLike n => NodeName -> n -> GraphPutRequest -> GraphPutRequest
addNodeLike name = addNode name . MergeN . toNode


addRelationLike :: URelationLike r => NodeName -> NodeName -> r -> GraphPutRequest -> GraphPutRequest
addRelationLike from to = addRelation from to . MergeR . toURelation


nodeRelList :: (NodeLike n, URelationLike r) => Direction -> Text -> [(n,r)] -> [GraphPutRequest -> GraphPutRequest]
nodeRelList dir prefix lst = 
  [ (directed addRelationLike name "reaction" rel) . (addNodeLike name node)
    | name <- indexedNames prefix
    | (node, rel) <- lst
  ] where
      directed f = case dir of
                    ToReaction   -> f
                    FromReaction -> flip f


getReaction :: Id Reaction -> BoltActionT IO (Maybe ReactionData)
getReaction i = do
  resp <- forM (getReactionDataGraphs i) $ makeRequest @GetRequest []
  let [reactResp, reagResp, prodResp, catResp] = resp
      rs             :: [Reaction]     = extractNode "reaction" <$> reactResp
      rdReagents     :: [Molecule]     = extractNode "reagent"  <$> reagResp
      productLst     :: [Molecule]     = extractNode "product"  <$> prodResp
      catalystLst    :: [Catalyst]     = extractNode "catalyst" <$> catResp
      productFromLst :: [PRODUCT_FROM] = extractRelation "reaction" "product"  <$> prodResp
      accelerateLst  :: [ACCELERATE]   = extractRelation "catalyst" "reaction" <$> catResp
      rdProducts = zip productLst productFromLst
      rdCatalyst = zip catalystLst accelerateLst
  case rs of
    []           -> pure Nothing
    [rdReaction] -> do liftIO $ print ReactionData{..}
                       pure $ Just ReactionData{..}

getReactionDataGraphs :: Id Reaction -> [GraphGetRequest]
getReactionDataGraphs i = [ getReactionGraph i
                          , getReagentsGraph i
                          , getProductsGraph i
                          , getCatalystsGraph i
                          ]


getReactionGraph :: Id Reaction -> GraphGetRequest
getReactionGraph (Id i) = emptyGraph & addNode "reaction" reactionNode
  where
    reactionNode = defaultNodeReturn & withLabelQ ''Reaction & withBoltId i & withReturn allProps


getReagentsGraph :: Id Reaction -> GraphGetRequest
getReagentsGraph (Id i) = emptyGraph & addNode "reaction" reactionNode
                                     & addNode "reagent" reagentNode
                                     & addRelation "reagent" "reaction" reagentRel
  where
    reactionNode = defaultNodeNotReturn  & withLabelQ ''Reaction & withBoltId i
    reagentNode  = defaultNodeReturn & withLabelQ ''Molecule & withReturn allProps
    reagentRel   = defaultRelNotReturn & withLabelQ ''REAGENT_IN


getProductsGraph :: Id Reaction -> GraphGetRequest
getProductsGraph (Id i) = emptyGraph & addNode "reaction" reactionNode
                                     & addNode "product" productNode
                                     & addRelation "reaction" "product" productRel
  where
    reactionNode = defaultNodeNotReturn & withLabelQ ''Reaction & withBoltId i
    productNode  = defaultNodeReturn & withLabelQ ''Molecule & withReturn allProps
    productRel   = defaultRelReturn & withLabelQ ''PRODUCT_FROM & withReturn allProps


getCatalystsGraph :: Id Reaction -> GraphGetRequest
getCatalystsGraph (Id i) = emptyGraph & addNode "reaction" reactionNode
                                      & addNode "catalyst" catalystNode
                                      & addRelation "catalyst" "reaction" accelerateRel
  where
    reactionNode  = defaultNodeNotReturn & withLabelQ ''Reaction & withBoltId i
    catalystNode  = defaultNodeReturn & withLabelQ ''Catalyst & withReturn allProps
    accelerateRel = defaultRelReturn & withLabelQ ''ACCELERATE & withReturn allProps


matchReactionNameGraph :: Reaction -> GraphGetRequest
matchReactionNameGraph Reaction{..} = emptyGraph & addNode "reaction" reactionNode
  where
    reactionNode = defaultNodeReturn & withLabelQ ''Reaction & withProp ("name" =: getName r'name) & withReturn allProps