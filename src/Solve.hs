module Solve
  ( brute
  , prop
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Map (Map)

import qualified Data.Map as Map

import Types (Formula(..), Variable(..), Clause(..), Literal(..))
import Common (simplify, variables, nullClauses)

brute :: Formula -> Maybe (Map Variable Bool)
brute f = solve' f Map.empty
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

prop :: Formula -> Maybe (Map Variable Bool)
prop f = solve' f Map.empty
  where
    solve' :: Formula -> Map Variable Bool -> Maybe (Map Variable Bool)
    solve' f assignments = do
      guard . not $ nullClauses f
      let (f', assignments') = propagateUnits (f, assignments)
      case variables f' of
        [] -> Just assignments'
        v : _ -> tryEither solve' v f' assignments'


tryEither ::
  (Formula -> Map Variable Bool -> Maybe (Map Variable Bool)) ->
  Variable ->
  Formula ->
  Map Variable Bool ->
  Maybe (Map Variable Bool)
tryEither solver v f assignments =
  solver (simplify v True f) (Map.insert v True assignments)
    <|> solver (simplify v False f) (Map.insert v False assignments)
