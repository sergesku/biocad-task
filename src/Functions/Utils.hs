{-# LANGUAGE ScopedTypeVariables #-}

module Functions.Utils 
  ( indexedNames
  , unpackSingleId
  , fromRelation
  , toMoleculeNode
  , toReactionNode
  , extractTransformation
  ) where

import Types

import Database.Bolt
import Database.Bolt.Extras
import Data.Text                  (Text, pack)
import Control.Applicative        (liftA2)
import Control.Monad.Error.Class  (throwError)

indexedNames :: Text -> [Text]
indexedNames n = map ((n <>) . pack . show) [1..]

unpackSingleId :: [Record] -> BoltActionT IO (Id a)
unpackSingleId (rec:_) = Id <$> rec `at` "id"
unpackSingleId [] = throwError NoStructureInResponse

fromRelation :: URelationLike a => Relationship -> a
fromRelation = fromURelation . convertRelType

convertRelType :: Relationship -> URelationship
convertRelType Relationship{..} = URelationship relIdentity relType relProps

toMoleculeNode :: Node -> PathNode
toMoleculeNode = liftA2 MoleculeNode (Id . getBoltId) fromNode

toReactionNode :: Node -> PathNode
toReactionNode = liftA2 ReactionNode (Id . getBoltId) fromNode

extractTransformation :: Record -> BoltActionT IO Transformation
extractTransformation record = do
  nodes :: [Node] <- record `at` "pathNodes"
  return $ zipWith ($) (cycle [toMoleculeNode, toReactionNode]) nodes