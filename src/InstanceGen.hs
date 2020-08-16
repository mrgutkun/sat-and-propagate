module InstanceGen where

import Control.Monad (replicateM)
import Control.Monad.Random.Lazy (evalRand, liftRand)
import System.Random (mkStdGen, randomR, random)

import Types(Formula(..), Clause(..), Literal(..), Variable(..))

genInstances :: Int -> Int -> [Formula]
genInstances seed n = evalRand (genF n) (mkStdGen seed)
  where 
    genF n = replicateM n $ Formula <$> replicateM n genClause
    genClause = Clause <$> replicateM 3 genLiteral
    genLiteral = Literal <$> genVariable <*> liftRand random
    genVariable = Variable <$> liftRand (randomR (0, n))