module Syntax.Term
  ( Term (..),
  )
where

import Syntax.Constant (FnConst, ObjConst)
import Syntax.Variable (Var)

-- | A first-order term which is intended to denote an "object" in the domain
-- being reasoned about. A term can be built up from "object"-denoting variables
-- and constants using functions.
data Term
  = TVar Var
  | TObj ObjConst
  | TFn FnConst [Term]
  deriving (Eq, Ord, Show)