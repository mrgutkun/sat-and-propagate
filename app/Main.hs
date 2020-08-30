module Main where

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Types as T
import qualified Solve
import qualified SolveM


import InstanceGen (GenParams(..), genInstances, easyParams, hardParams)

main :: IO ()
main = undefined

test :: GenParams ->
  ( [Maybe (T.Variable, Bool)]
  , [Maybe (T.Variable, Bool)]
  , [Maybe (T.Variable, Bool)]
  , [Maybe (T.Variable, Bool)]
  )
test params =
  let brute  = probeSolver params Solve.brute
      bruteM = probeSolver params SolveM.brute
      prop   = probeSolver params Solve.prop
      propM  = probeSolver params SolveM.prop
  in (brute, bruteM, prop, propM)

probeSolver :: Functor f => GenParams -> ((T.Formula -> f (Map k a)) -> [f (k, a)])
probeSolver params = map (fmap $ Map.elemAt 1) . testSolver params

testSolver :: GenParams -> (T.Formula -> b) -> [b]
testSolver params solver  =
  map solver $ genInstances params

example :: T.Formula
example =
  T.Formula
    [ T.Clause
        [ T.Literal (T.Variable 3) True
        , T.Literal (T.Variable 2) False
        ]
    , T.Clause
        [ T.Literal (T.Variable 3) False ]
    ]