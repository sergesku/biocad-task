module SampleData
  ( randomReaction
  ) where

import Types

import Data.Maybe               (catMaybes)
import System.Random            (randomRIO)
import Data.Text.IO as TIO      (readFile)
import Data.Text                (Text, splitOn, lines)
import Control.Applicative      (liftA2)
import Control.Monad            (replicateM)
import qualified Data.Text as T (lines)


parseReaction :: Text -> Reaction
parseReaction = Reaction . Name


parseMolecule :: Text -> Maybe Molecule
parseMolecule txt = case splitOn "; " txt of
                      (smiles:iupacName:_) -> Just $ Molecule (Smiles smiles) (Name iupacName)
                      _                    -> Nothing


parseCatalyst :: Text -> Maybe Catalyst
parseCatalyst txt = case splitOn "; " txt of
                      []              -> Nothing
                      [smiles]        -> Just $ Catalyst (Smiles smiles) Nothing
                      (smiles:name:_) -> Just $ Catalyst (Smiles smiles) (Just $ Name name)


readReactions :: IO [Reaction]
readReactions = do
  input <- TIO.readFile "./SampleData/Reactions.csv"
  pure $ fmap parseReaction. tail . T.lines $ input


readMolecules :: IO [Molecule]
readMolecules = do 
  input <- TIO.readFile "./SampleData/Molecules.csv"
  pure $ catMaybes . fmap parseMolecule . tail . T.lines $ input


readCatalysts :: IO [Catalyst]
readCatalysts = do 
  input <- TIO.readFile "./SampleData/Catalysts.csv"
  pure $ catMaybes . fmap parseCatalyst. tail . T.lines $ input


randomListElement :: [a] -> IO a
randomListElement lst = randomRIO (0,n) >>= pure . (lst !!)
  where n = length lst - 1


randomAccelerate :: IO ACCELERATE
randomAccelerate = do
  a'temperature <- (Temp . (/10). fromIntegral) <$> randomRIO (1,10000 :: Integer)
  a'pressure    <- (Pressure . (/10). fromIntegral) <$> randomRIO (1,100 :: Integer)
  pure ACCELERATE{..}


randomProductFrom :: IO PRODUCT_FROM
randomProductFrom = (PRODUCT_FROM . Amount . (/100) . fromIntegral) <$> randomRIO (1,10000 :: Integer)


randomReaction :: IO ReactionData
randomReaction = do
  reacLst <- readReactions
  catLst  <- readCatalysts
  molLst  <- readMolecules
  reagentN  <- (ceiling . (*3)) <$> randomRIO (0.2, 0.8 :: Float)
  productN  <- (ceiling . (*3)) <$> randomRIO (0.2, 0.8 :: Float)
  catalystN <- (floor . (*3)) <$> randomRIO (0.3, 0.8 :: Float)
  rdReaction <- randomListElement reacLst
  rdReagents <- replicateM reagentN (randomListElement molLst)
  rdProducts <- replicateM productN $ liftA2 (,) (randomListElement molLst) randomProductFrom
  rdCatalyst <- replicateM catalystN $ liftA2 (,) (randomListElement catLst) randomAccelerate
  pure ReactionData{..}