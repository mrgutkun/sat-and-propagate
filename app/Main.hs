module Main where

import Types
import Solvers

import InstanceGen (genInstances)

main :: IO ()
main = undefined

example :: Formula
example =
  Formula
    [ Clause [Literal (Variable 3) True, Literal (Variable 2) False],
      Clause [Literal (Variable 3) False]
    ]