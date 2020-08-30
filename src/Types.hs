{-# LANGUAGE LambdaCase #-}
module Types where

import Data.List (intercalate)

newtype Formula = Formula { f_clauses :: [Clause] }
  deriving (Eq)

instance Show Formula where
  show = \case
    Formula []  -> "()"
    Formula cls ->
      intercalate " /\\ " $
        map (\cl -> "(" <> show cl <> ")") cls

newtype Clause = Clause { c_literals :: [Literal] }
  deriving (Eq)

instance Show Clause where
  show (Clause ls) = intercalate " \\/ " $ map show ls

data Literal = Literal
  { l_variable :: Variable
  , l_value :: Bool
  }
  deriving (Eq)

instance Show Literal where
  show = \case
    Literal l True  -> show l
    Literal l False -> "¬" <> show l

newtype Variable = Variable Int
  deriving (Eq, Ord)

instance Show Variable where
  show (Variable s) = "x_" <> show s
