{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text (Text)


newtype Smiles a = Smiles {getSmiles :: Text} deriving (Show, Eq)
newtype Name a   = Name {getName :: Text} deriving (Show, Eq)
newtype Amount   = Amount {getAmount :: Float} deriving (Show, Eq)
newtype Temp     = Temp {getTemp :: Float} deriving (Show, Eq)
newtype Pressure = Pressure {getPressure :: Float} deriving (Show, Eq)

data Molecule = Molecule
	{ mSmiles :: Smiles Molecule
	, mName   :: Name Molecule
	} deriving (Show, Eq)

data ReactionProduct = ReactionProduct
  { pMolecule :: Molecule
  , pAmount   :: Amount
  } deriving (Show, Eq)

data Catalyst = Catalyst
  { cSmiles :: Smiles Catalyst
  , cName   :: Maybe (Name Catalyst)
  } deriving (Show, Eq)

data Conditions = Conditions
  { temp :: Temp
  , pressure :: Pressure
  } deriving (Show, Eq)

data Accelerant = Accelerant
  { aCatalyst   :: Catalyst
  , aConditions :: Conditions
  } deriving (Show, Eq)

data Reaction = Reaction
  { rName       :: Name Reaction
  , rReagents   :: [Molecule]
  , rProducts   :: [ReactionProduct] 
  , rAccelerant :: Maybe Accelerant
  }