{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.Text                      (Text)
import Data.List                      (sort)
import Database.Bolt            
import Database.Bolt.Extras
import Database.Bolt.Extras.Template

type Transformation = [PathNode]

newtype Id a     = Id {getId :: Int}deriving (Eq, Ord, Show, Read, ToValue, FromValue)
newtype Smiles a = Smiles {getSmiles :: Text} deriving (Eq, Ord, Show, Read, ToValue, FromValue)
newtype Name a   = Name {getName :: Text} deriving (Eq, Ord, Show, Read, ToValue, FromValue)
newtype Amount   = Amount {getAmount :: Double} deriving (Eq, Ord, Show, Read, ToValue, FromValue)
newtype Temp     = Temp {getTemp :: Double} deriving (Eq, Ord, Show, Read, ToValue, FromValue)
newtype Pressure = Pressure {getPressure :: Double} deriving (Eq, Ord, Show, Read, ToValue, FromValue)

data Molecule = Molecule
  { m'smiles    :: Smiles Molecule
  , m'iupacName :: Name Molecule
  } deriving (Eq, Ord, Show, Read)

data Catalyst = Catalyst
  { c'smiles :: Smiles Catalyst
  , c'name   :: Maybe (Name Catalyst)
  } deriving (Eq, Ord, Show, Read)

data Reaction = Reaction
  { r'name :: Name Reaction
  } deriving (Eq, Ord, Show, Read)

data PRODUCT_FROM = PRODUCT_FROM
  { p'amount :: Amount
  } deriving (Eq, Ord, Show, Read)

data REAGENT_IN = REAGENT_IN deriving (Eq, Ord, Show, Read)

data ACCELERATE = ACCELERATE
  { a'temperature :: Temp
  , a'pressure    :: Pressure
  } deriving (Eq, Ord, Show, Read)

data ReactionData = ReactionData
  { rdReaction  :: Reaction
  , rdReagents  :: [Molecule]
  , rdProducts  :: [(Molecule, PRODUCT_FROM)]
  , rdCatalyst  :: [(Catalyst, ACCELERATE)]
  } deriving (Show, Read)

data Direction = ToReaction
               | FromReaction
               deriving (Eq, Show, Read)

data PathNode = MoleculeNode (Id Molecule) Molecule
              | ReactionNode (Id Reaction) Reaction
              deriving (Eq, Ord, Show, Read)


fromRelation :: URelationLike a => Relationship -> a
fromRelation = fromURelation . convertRelType
  where convertRelType Relationship{..} = URelationship relIdentity relType relProps


instance NodeLike PathNode where
  fromNode node | "Molecule" `elem` labels = MoleculeNode (Id idNode) (fromNode node)
                | "Reaction" `elem`labels  = ReactionNode (Id idNode) (fromNode node)
                | otherwise = error $ "Could not unpack PathNode from " ++ show node
                where labels = getLabels node
                      idNode = getBoltId node
  toNode (MoleculeNode (Id i) m) = Node i ["Molecule"] (getProps $ toNode m)
  toNode (ReactionNode (Id i) r) =  Node i ["Reaction"] (getProps $ toNode r)


instance Eq ReactionData where
  rd1 == rd2 = (rdReaction rd1 == rdReaction rd2)
                  && (sort (rdReagents rd1) == sort (rdReagents rd2))
                  && (sort (rdProducts rd1) == sort (rdProducts rd2))
                  && (sort (rdCatalyst rd1) == sort (rdCatalyst rd2))

instance RecordValue Reaction where exactEither = fmap fromNode . exactEither
instance RecordValue Molecule where exactEither = fmap fromNode . exactEither
instance RecordValue Catalyst where exactEither = fmap fromNode . exactEither
instance RecordValue PathNode where exactEither = fmap fromNode . exactEither

instance RecordValue REAGENT_IN   where exactEither = fmap fromRelation . exactEither
instance RecordValue PRODUCT_FROM where exactEither = fmap fromRelation . exactEither
instance RecordValue ACCELERATE   where exactEither = fmap fromRelation . exactEither

instance RecordValue a => RecordValue (Id a) where exactEither = fmap Id . exactEither


makeURelationLike ''REAGENT_IN
makeURelationLikeWith ''ACCELERATE $ drop 2
makeURelationLikeWith ''PRODUCT_FROM $ drop 2
makeNodeLikeWith ''Molecule $ drop 2
makeNodeLikeWith ''Catalyst $ drop 2
makeNodeLikeWith ''Reaction $ drop 2