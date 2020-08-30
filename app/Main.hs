module Main where

import Data.Map (Map)
import qualified Data.Map as Map

import Types
import qualified Solve
import qualified SolveM


import InstanceGen (genInstances)

main :: IO ()
main = undefined

test :: Int ->
  ( [Maybe (Variable, Bool)]
  , [Maybe (Variable, Bool)]
  , [Maybe (Variable, Bool)]
  , [Maybe (Variable, Bool)]
  )
test n =
  let brute  = probeSolver n Solve.brute
      bruteM = probeSolver n SolveM.brute
      prop   = probeSolver n Solve.prop
      propM  = probeSolver n SolveM.prop
  in (brute, bruteM, prop, propM)

probeSolver :: Functor f => Int -> ((Formula -> f (Map k a)) -> [f (k, a)])
probeSolver n = map (fmap $ Map.elemAt 1) . testSolver n

testSolver :: Int -> (Formula -> b) -> [b]
testSolver n s = map s $ genInstances n n

example :: Formula
example =
  Formula
    [ Clause [Literal (Variable 3) True, Literal (Variable 2) False],
      Clause [Literal (Variable 3) False]
    ]