{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
module SolveM
  ( brute
  , prop
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

data LocalState = LocalState
  { s_formula :: Formula
  , s_evaluation :: Map Variable Bool
  }

type Solve = State LocalState

brute :: Formula -> Maybe (Map Variable Bool)
brute f = solve (LocalState f Map.empty)
  where
    solve :: LocalState -> Maybe (Map Variable Bool)
    solve s@LocalState{..} = do
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


prop :: Formula -> Maybe (Map Variable Bool)
prop f = solve $ LocalState f Map.empty
  where
    solve :: LocalState -> Maybe (Map Variable Bool)
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
  -> LocalState
  -> (LocalState, LocalState)
withEither var s@LocalState{..} =
  (withTrue, withFalse)
  where
    withTrue = execState (withVarValue var True) s
    withFalse = execState (withVarValue var False) s

withVarValue :: Variable -> Bool -> Solve ()
withVarValue var value = modify $ \LocalState{..} ->
  LocalState
    { s_formula    = simplify var value s_formula
    , s_evaluation = Map.insert var value s_evaluation
    }