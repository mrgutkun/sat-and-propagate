{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
module SolverM
  ( solveBrute
  , solveProp
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM_, guard)
import Control.Monad.Trans.State
  (modify, gets, State, evalState, runState, state, execState)

import Data.Map (Map)

import qualified Data.Map as Map

import Types (Formula(..), Variable(..), Clause(..), Literal(..))
import Common(simplify, variables, nullClauses)

data SolveState = SolveState
  { s_formula :: Formula
  , s_evaluation :: Map Variable Bool
  }

type Solve = State SolveState

solveBrute :: Formula -> Maybe (Map Variable Bool)
solveBrute f = solve (SolveState f Map.empty)
  where
    solve :: SolveState -> Maybe (Map Variable Bool)
    solve s@SolveState{..} = do
      guard . not $ nullClauses s_formula
      case variables s_formula of
        [] -> Just s_evaluation
        v : _ ->
          let (sLeft, sRight) = withEither v s
            in solve sLeft <|> solve sRight

propagateUnits :: Solve ()
propagateUnits = whileM areUnits propagateOnce

areUnits :: Solve Bool
areUnits = gets $ null . getUnits . s_formula

propagateOnce :: Solve ()
propagateOnce = do
  units <- gets $ getUnits . s_formula
  forM_ units propagateUnit

propagateUnit :: Literal -> Solve ()
propagateUnit Literal{..} = withVarValue l_variable l_value

getUnits :: Formula -> [Literal]
getUnits Formula{..} = do
  Clause [l] <- f_clauses
  pure l

whileM :: Monad m => m Bool -> m () -> m ()
whileM prop action = do
  cond <- prop
  if cond
    then action >> whileM prop action
    else pure ()


solveProp :: Formula -> Maybe (Map Variable Bool)
solveProp f = solve $ SolveState f Map.empty
  where
    solve :: SolveState -> Maybe (Map Variable Bool)
    solve s = do
      guard . not . nullClauses $ s_formula s
      let s' = execState propagateUnits s
      case variables $ s_formula s' of
        [] -> Just $ s_evaluation s'
        v : _ ->
          let (sLeft, sRight) = withEither v s
            in solve sLeft <|> solve sRight

withEither
  :: Variable
  -> SolveState
  -> (SolveState, SolveState)
withEither var s@SolveState{..} =
  (withTrue, withFalse)
  where
    withTrue = execState (withVarValue var True) s
    withFalse = execState (withVarValue var False) s

withVarValue :: Variable -> Bool -> Solve ()
withVarValue var value = modify $ \SolveState{..} ->
  SolveState
    { s_formula    = simplify var value s_formula
    , s_evaluation = Map.insert var value s_evaluation
    }