module Types where

import Data.List (intercalate)

data Formula = Formula { f_clauses :: [Clause]}
  deriving (Eq)

instance Show Formula where
  show (Formula []) = "()"
  show (Formula cls) =
    intercalate " /\\ " $
      map (\cl -> "(" <> show cl <> ")") cls

data Clause = Clause {c_literals :: [Literal]}
  deriving (Eq)

instance Show Clause where
  show (Clause ls) = intercalate " \\/ " $ map show ls

data Literal = Literal 
  { l_variable :: Variable
  , l_unvalue :: Bool
  }
  deriving (Eq)

instance Show Literal where
  show (Literal l True) = show l
  show (Literal l False) = "¬" <> show l

newtype Variable = Variable Int
  deriving (Eq, Ord)

instance Show Variable where
  show (Variable s) = "x_" <> show s
