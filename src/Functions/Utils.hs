{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Functions.Utils 
  ( indexedNames
  , unpackSingleId
  , fromRelation
  , toMoleculeNode
  , toReactionNode
  ) where

import Types

import Data.Text      (Text, pack)
import Database.Bolt
import Database.Bolt.Extras
import Control.Applicative (liftA2)
import Control.Monad.Error.Class

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