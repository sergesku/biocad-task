{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SampleData where

import Types
import Functions.TextQuery
import Functions.GraphQuery

import Data.Text (Text)
import qualified Data.Text as T

parseReaction :: Text -> Reaction
parseReaction = Reaction . Name


parseMolecule :: Text -> Maybe Molecule
parseMolecule txt = case splitOn "; " txt of
                      [smiles, iupacName] -> Just $ Molecule (Smiles smiles) (Name iupacName)
                      _                   -> Nothing


parseCatalyst :: Text -> Maybe Catalyst
parseCatalyst txt = case splitOn "; " txt of
                      [smiles] -> Catalyst (Smiles smiles) Nothing
                      [smiles, name] -> Catalyst (Smiles smiles) (Just $ Name name)
                      _              -> Nothing

