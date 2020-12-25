{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Functions.GraphRequest
  ( putReaction
  , getReaction
  ) where

import Types
import Functions.GraphRequest.Internal

import Control.Monad                (forM)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Error.Class    (throwError)
import Database.Bolt
import Database.Bolt.Extras.Graph
import qualified Data.Map as M      (lookup) 


putReaction :: ReactionData -> BoltActionT IO (Id Reaction)
putReaction rd@ReactionData{..} = do 
  reactGraph <- makeRequest @GetRequest [] (matchReactionNameGraph rdReaction)
  let rs :: [Reaction] = extractNode "reaction" <$> reactGraph
      (idr:_) = extractNodeId "reaction" <$> reactGraph
  case rs of
    [] -> do graphs <- makeRequest @PutRequest [] (putReactionGraph rd)
             let mbI = M.lookup "reaction" (_vertices (head graphs))
             case mbI of
               Just i  -> pure $ Id i
               Nothing -> throwError NoStructureInResponse
    _  -> do liftIO $ print $ "Warning | Reaction " ++ (show . getName . r'name $ rdReaction) ++ " already exists. Id: " ++ (show idr)
             pure $ Id idr


getReaction :: Id Reaction -> BoltActionT IO (Maybe ReactionData)
getReaction i = do
  resp <- forM (getReactionDataGraphs i) $ makeRequest @GetRequest []
  let [reactResp, reagResp, prodResp, catResp] = resp
      rs             :: [Reaction]     = extractNode "reaction" <$> reactResp
      rdReagents     :: [Molecule]     = extractNode "reagent"  <$> reagResp
      productLst     :: [Molecule]     = extractNode "product"  <$> prodResp
      catalystLst    :: [Catalyst]     = extractNode "catalyst" <$> catResp
      productFromLst :: [PRODUCT_FROM] = extractRelation "reaction" "product"  <$> prodResp
      accelerateLst  :: [ACCELERATE]   = extractRelation "catalyst" "reaction" <$> catResp
      rdProducts = zip productLst productFromLst
      rdCatalyst = zip catalystLst accelerateLst
  case rs of
    [rdReaction] -> pure $ Just ReactionData{..}
    _            -> pure Nothing
