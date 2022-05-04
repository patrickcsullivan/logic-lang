module Syntax.Variable
  ( Var (..),
  )
where

newtype Var = Var
  { varName :: String
  }
  deriving (Eq, Ord)

instance Show Var where
  show (Var var) = var
