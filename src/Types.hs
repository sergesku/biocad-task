{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Database.Bolt.Extras
import Database.Bolt.Extras.Template
import Data.Text (Text)

newtype Id a     = Id {getId :: Int} deriving (Show, Eq)
newtype Smiles a = Smiles {getSmiles :: Text} deriving (Show, Eq)
newtype Name a   = Name {getName :: Text} deriving (Show, Eq)
newtype Amount   = Amount {getAmount :: Float} deriving (Show, Eq)
newtype Temp     = Temp {getTemp :: Float} deriving (Show, Eq)
newtype Pressure = Pressure {getPressure :: Float} deriving (Show, Eq)


instance FromValue (Id a)     where fromValue = Id . fromValue
instance FromValue (Smiles a) where fromValue = Smiles . fromValue
instance FromValue (Name a)   where fromValue = Name . fromValue
instance FromValue Amount     where fromValue = Amount . fromValue
instance FromValue Temp       where fromValue = Temp . fromValue
instance FromValue Pressure   where fromValue = Pressure . fromValue
instance ToValue (Id a)       where toValue = toValue . getId
instance ToValue (Smiles a)   where toValue = toValue . getSmiles
instance ToValue (Name a)     where toValue = toValue . getName
instance ToValue Amount       where toValue = toValue . getAmount
instance ToValue Temp         where toValue = toValue . getTemp
instance ToValue Pressure     where toValue = toValue . getPressure


data Molecule = Molecule
	{ m'smiles    :: Smiles Molecule
	, m'iupacName :: Name Molecule
	} deriving (Show, Eq)

data Catalyst = Catalyst
  { c'smiles :: Smiles Catalyst
  , c'name   :: Maybe (Name Catalyst)
  } deriving (Show, Eq)

data Reaction = Reaction
  { r'name :: Name Reaction
  } deriving (Show, Eq)

data PRODUCT_FROM = PRODUCT_FROM
  { p'amount :: Amount
  } deriving (Show, Eq)

data REAGENT_IN = REAGENT_IN deriving (Show, Eq)

data ACCELERATE = ACCELERATE
  { a'temperature :: Temp
  , a'pressure    :: Pressure
  } deriving (Show, Eq)

data ReactionData = ReactionData
  { rd'name     :: Reaction
  , rd'reagents :: [Molecule]
  , r'products  :: [(Molecule, PRODUCT_FROM)] 
  , r'catalyst  :: Maybe (Catalyst, ACCELERATE)
  }

makeURelationLike ''REAGENT_IN
makeURelationLikeWith ''ACCELERATE $ drop 2
makeURelationLikeWith ''PRODUCT_FROM $ drop 2
makeNodeLikeWith ''Molecule $ drop 2
makeNodeLikeWith ''Catalyst $ drop 2
makeNodeLikeWith ''Reaction $ drop 2