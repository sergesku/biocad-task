{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.Text                      (Text)
import Database.Bolt            
import Database.Bolt.Extras
import Database.Bolt.Extras.Template

type Transformation = [PathNode]

newtype Id a     = Id {getId :: Int}deriving (Eq, Show, Read, ToValue, FromValue)
newtype Smiles a = Smiles {getSmiles :: Text} deriving (Eq, Show, Read, ToValue, FromValue)
newtype Name a   = Name {getName :: Text} deriving (Eq, Show, Read, ToValue, FromValue)
newtype Amount   = Amount {getAmount :: Double} deriving (Eq, Show, Read, ToValue, FromValue)
newtype Temp     = Temp {getTemp :: Double} deriving (Eq, Show, Read, ToValue, FromValue)
newtype Pressure = Pressure {getPressure :: Double} deriving (Eq, Show, Read, ToValue, FromValue)

data Molecule = Molecule
	{ m'smiles    :: Smiles Molecule
	, m'iupacName :: Name Molecule
	} deriving (Eq, Show, Read)

data Catalyst = Catalyst
  { c'smiles :: Smiles Catalyst
  , c'name   :: Maybe (Name Catalyst)
  } deriving (Eq, Show, Read)

data Reaction = Reaction
  { r'name :: Name Reaction
  } deriving (Eq, Show, Read)

data PRODUCT_FROM = PRODUCT_FROM
  { p'amount :: Amount
  } deriving (Eq, Show, Read)

data REAGENT_IN = REAGENT_IN deriving (Eq, Show, Read)

data ACCELERATE = ACCELERATE
  { a'temperature :: Temp
  , a'pressure    :: Pressure
  } deriving (Eq, Show, Read)

data ReactionData = ReactionData
  { rdReaction  :: Reaction
  , rdReagents  :: [Molecule]
  , rdProducts  :: [(Molecule, PRODUCT_FROM)]
  , rdCatalyst  :: [(Catalyst, ACCELERATE)]
  } deriving (Eq, Show, Read)

data Direction = ToReaction
               | FromReaction
               deriving (Eq, Show, Read)

data PathNode = MoleculeNode (Id Molecule) Molecule
              | ReactionNode (Id Reaction) Reaction
              deriving (Eq, Show, Read)


instance NodeLike PathNode where
  fromNode node | "Molecule" `elem` labels = MoleculeNode (Id idNode) (fromNode node)
                | "Reaction" `elem`labels  = ReactionNode (Id idNode) (fromNode node)
                | otherwise = error $ "Could not unpack PathNode from " ++ show node
                where labels = getLabels node
                      idNode = getBoltId node
  toNode (MoleculeNode (Id i) m) = Node i ["Molecule"] (getProps $ toNode m)
  toNode (ReactionNode (Id i) r) =  Node i ["Reaction"] (getProps $ toNode r)


instance RecordValue Reaction where exactEither = fmap fromNode . exactEither
instance RecordValue Molecule where exactEither = fmap fromNode . exactEither
instance RecordValue Catalyst where exactEither = fmap fromNode . exactEither
instance RecordValue PathNode where exactEither = fmap fromNode . exactEither

instance RecordValue REAGENT_IN   where exactEither = fmap fromURelation . exactEither
instance RecordValue PRODUCT_FROM where exactEither = fmap fromURelation . exactEither
instance RecordValue ACCELERATE   where exactEither = fmap fromURelation . exactEither


makeURelationLike ''REAGENT_IN
makeURelationLikeWith ''ACCELERATE $ drop 2
makeURelationLikeWith ''PRODUCT_FROM $ drop 2
makeNodeLikeWith ''Molecule $ drop 2
makeNodeLikeWith ''Catalyst $ drop 2
makeNodeLikeWith ''Reaction $ drop 2