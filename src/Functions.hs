{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Functions where

import Types
import Control.Monad (void, forM)
import Control.Monad.IO.Class
import Data.List (foldl')
import Data.Text (Text, pack)
import Data.Function ((&))
import Database.Bolt
import Database.Bolt.Extras
import Database.Bolt.Extras.Graph

findShortPath :: Molecule -> Molecule -> Path
findShortPath = undefined

putReaction :: ReactionData -> BoltActionT IO ()
putReaction = void . makeRequest @PutRequest [] . putReactionGraph

indNames :: NodeName -> [NodeName]
indNames n = map ((n <>) . pack . show) [1..]

addNodeLike :: NodeLike n => NodeName -> n -> GraphPutRequest -> GraphPutRequest
addNodeLike name = addNode name . MergeN . toNode

addRelationLike :: URelationLike r => NodeName -> NodeName -> r -> GraphPutRequest -> GraphPutRequest
addRelationLike from to = addRelation from to . MergeR . toURelation

nodeRelList :: (NodeLike n, URelationLike r) => Direction -> Text -> [(n,r)] -> [GraphPutRequest -> GraphPutRequest]
nodeRelList dir prefix lst = [ (directed addRelationLike name "reaction" rel) . (addNodeLike name node)
                             | name <- indNames prefix
                             | (node, rel) <- lst
                             ]
                            where 
                              directed f = case dir of
                                ToReaction   -> f
                                FromReaction -> flip f
                           
putReactionGraph :: ReactionData -> GraphPutRequest
putReactionGraph ReactionData{..} = foldl' (&) emptyGraph $ concat [reactionNode, reagentList, productList, catalystList]
  where
    reactionNode = [addNodeLike "reaction" rdReaction]
    reagentList  = nodeRelList ToReaction   "reagent"  $ zip rdReagents (repeat REAGENT_IN)
    productList  = nodeRelList FromReaction "product"  rdProducts
    catalystList = nodeRelList ToReaction   "catalyst" rdCatalyst


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
    []           -> return Nothing
    [rdReaction] -> do liftIO $ print ReactionData{..}
                       return $ Just ReactionData{..}

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