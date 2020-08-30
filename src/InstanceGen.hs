{-# LANGUAGE RecordWildCards #-}
module InstanceGen where

import Control.Monad (replicateM)
import Control.Monad.Random.Lazy (evalRand, liftRand)
import System.Random (mkStdGen, randomR, random)

import qualified Types as T

data GenParams = GenParams
  { gp_seed     :: Int
  , gp_formulas :: Int
  , gp_clauses  :: Int
  , gp_vars     :: Int
  }

easyParams :: Int -> GenParams
easyParams n =
  GenParams
    { gp_seed     = n
    , gp_formulas = n
    , gp_clauses  = n
    , gp_vars     = n
    }

hardParams :: Int -> GenParams
hardParams n =
  GenParams
    { gp_seed     = n
    , gp_formulas = n
    , gp_clauses  = n * 2
    , gp_vars     = n
    }

genInstances :: GenParams -> [T.Formula]
genInstances GenParams{..} = evalRand genF (mkStdGen gp_seed)
  where
    genF = replicateM gp_formulas $ T.Formula <$> replicateM gp_clauses genClause
    genClause = T.Clause <$> replicateM 3 genLiteral
    genLiteral = T.Literal <$> genVariable <*> liftRand random
    genVariable = T.Variable <$> liftRand (randomR (0, gp_vars))