{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeApplications #-}

module Functions where

import Types
import Data.List (foldl')
import Data.Text (Text, pack)
import Data.Function ((&))
import Database.Bolt
import Database.Bolt.Extras
import Database.Bolt.Extras.Graph

putReaction :: ReactionData -> BoltActionT IO ()
putReaction = makeRequest @PutRequest [] . putReactionGraph

getReaction :: Id Reaction -> Reaction
getReaction = undefined

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