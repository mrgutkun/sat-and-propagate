module Solvers
  ( solveBrute
  , solveProp
  )
where

import Control.Monad (guard)
import Data.Map (Map)

import qualified Data.Map as Map

import Types (Formula(..), Variable(..), Clause(..), Literal(..))
import Common(simplify, variables, nullClauses, tryEither)

solveBrute :: Formula -> Maybe (Map Variable Bool)
solveBrute f = solve' f Map.empty
  where
    solve' :: Formula -> Map Variable Bool -> Maybe (Map Variable Bool)
    solve' f assignments = do
      guard . not $ nullClauses f
      case variables f of
        [] -> Just assignments
        v : _ -> tryEither solve' v f assignments

propagateUnits :: (Formula, Map Variable Bool) -> (Formula, Map Variable Bool)
propagateUnits (f@(Formula cls), assignments) =
  case units of
    [] -> (f, assignments)
    _ -> propagateUnits $ foldr propagate (f, assignments) units
  where
    units = [l | Clause [l] <- cls]
    propagate :: Literal -> (Formula, Map Variable Bool) -> (Formula, Map Variable Bool)
    propagate (Literal var val) (f, assignments) =
      (simplify var val f, Map.insert var val assignments)

solveProp :: Formula -> Maybe (Map Variable Bool)
solveProp f = solve' f Map.empty
  where
    solve' :: Formula -> Map Variable Bool -> Maybe (Map Variable Bool)
    solve' f assignments = do
      guard . not $ nullClauses f
      let (f', assignments') = propagateUnits (f, assignments)
      case variables f' of
        [] -> Just assignments'
        v : _ -> tryEither solve' v f' assignments'