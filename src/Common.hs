{-# LANGUAGE RecordWildCards #-}
module Common where

import Control.Applicative ((<|>))

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map

import Types (Formula(..), Variable, Clause(..), Literal(..))

tryEither ::
  (Formula -> Map Variable Bool -> Maybe (Map Variable Bool)) ->
  Variable ->
  Formula ->
  Map Variable Bool ->
  Maybe (Map Variable Bool)
tryEither solver v f assignments =
  solver (simplify v True f) (Map.insert v True assignments)
    <|> solver (simplify v False f) (Map.insert v False assignments)


variables :: Formula -> [Variable]
variables Formula{..} = nub $ concatMap variables' f_clauses
  where
    variables' :: Clause -> [Variable]
    variables' Clause{..} = map l_variable c_literals

simplify :: Variable -> Bool -> Formula -> Formula
simplify var value (Formula cls) =
  Formula $ map simplify' $ filter (not . satisfied) cls
  where
    satisfied :: Clause -> Bool
    satisfied (Clause lits) =
      Literal var value `elem` lits

    simplify' :: Clause -> Clause
    simplify' (Clause lits) =
      Clause $ filter ((/= var) . l_variable) lits

nullClauses :: Formula -> Bool
nullClauses Formula{..} = any (null . c_literals) f_clauses