{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Functions where

import Types
import Control.Monad (void)
import Data.List (foldl')
import Data.Text (Text, pack)
import Data.Function ((&))
import Database.Bolt
import Database.Bolt.Extras
import Database.Bolt.Extras.Graph

putReaction :: ReactionData -> BoltActionT IO ()
putReaction = void . makeRequest @PutRequest [] . putReactionGraph

getReaction :: Id Reaction -> BoltActionT IO ReactionData
getReaction = undefined -- resp <- makeRequest @GetReguest [] . getReactionGraph


findShortPath :: Molecule -> Molecule -> Path
findShortPath = undefined


indNames :: NodeName -> [NodeName]
indNames n = map ((n <>) . pack . show) [1..]

addNodeLike :: NodeLike n => NodeName -> n -> GraphPutRequest -> GraphPutRequest
addNodeLike name = addNode name . MergeN . toNode

addRelationLike :: URelationLike r => NodeName -> NodeName -> r -> GraphPutRequest -> GraphPutRequest
addRelationLike from to = addRelation from to . MergeR . toURelation

nodeRelList :: (NodeLike n, URelationLike r) => Direction -> Text -> [(n,r)] -> [GraphPutRequest -> GraphPutRequest]
nodeRelList dir prefix lst = [ (directed addRelationLike name "reaction" rel) . (addNodeLike name node)
                             | name <- indNames prefix
                             | (node, rel) <- lst ]
                            where 
                              directed f = case dir of
                                ToReaction   -> f
                                FromReaction -> flip f
                           
putReactionGraph :: ReactionData -> GraphPutRequest
putReactionGraph ReactionData{..} = foldl' (&) emptyGraph $ concat [reactionNode, reagentList, productList, catalystList]
  where
    reactionNode = [addNodeLike "reaction" rdReaction]
    reagentList  = nodeRelList ToReaction "reagent" $ zip rdReagents (repeat REAGENT_IN)
    productList  = nodeRelList FromReaction "product" rdProducts
    catalystList = nodeRelList ToReaction "catalyst" rdCatalyst


getReactionGraph :: Id Reaction -> GraphGetRequest
getReactionGraph (Id i) = emptyGraph 
                            & addNode "reaction" reactionNode
                            & addNode "reagent" reagentNode
                            & addNode "product" reactionNode
                            & addNode "reaction" productNode
                            & addNode "catalyst" catalystNode
                            & addRelation "reagent" "reaction" reagentRel
                            & addRelation "reaction" "product" productRel
                            & addRelation "catalyst" "reaction" accelerateRel
  where
    reactionNode  = defaultNodeReturn & withLabelQ ''Reaction & withBoltId i & withReturn allProps
    reagentNode   = defaultNodeReturn & withLabelQ ''Molecule & withReturn allProps
    productNode   = defaultNodeReturn & withLabelQ ''Molecule & withReturn allProps
    catalystNode  = defaultNodeReturn & withLabelQ ''Catalyst & withReturn allProps
    reagentRel    = defaultRelReturn & withLabelQ ''REAGENT_IN
    productRel    = defaultRelReturn & withLabelQ ''PRODUCT_FROM
    accelerateRel = defaultRelReturn & withLabelQ ''ACCELERATE

    
