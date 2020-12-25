{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TemplateHaskell #-}

module Functions.GraphRequest.Internal where

import Types

import Data.List                    (foldl')
import Data.Text                    (Text, pack)
import Data.Function                ((&))
import Database.Bolt
import Database.Bolt.Extras
import Database.Bolt.Extras.Graph


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
      indexedNames :: NodeName -> [NodeName]
      indexedNames n = map ((n <>) . pack . show) [(1 :: Int)..]
      directed :: (a -> a -> b) -> (a -> a -> b)
      directed f = case dir of
                    ToReaction   -> f
                    FromReaction -> flip f


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