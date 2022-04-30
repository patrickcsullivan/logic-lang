module Syntax.Variable
  ( Var (..),
  )
where

newtype Var = Var
  { varName :: String
  }
  deriving (Eq, Ord, Show)
