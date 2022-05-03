module Syntax.Term
  ( Term (..),
    vars,
    objConsts,
    fnConsts,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
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

-- | Return the set of variables in the term.
vars :: Term -> Set Var
vars trm = case trm of
  TVar var -> Set.singleton var
  TObj _ -> Set.empty
  TFn _ args -> Set.unions (vars <$> args)

-- | Return the set of object constants in the term.
objConsts :: Term -> Set ObjConst
objConsts trm = case trm of
  TVar _ -> Set.empty
  TObj objConst -> Set.singleton objConst
  TFn _ args -> Set.unions (objConsts <$> args)

-- | Return the set of function constants in the term.
fnConsts :: Term -> Set FnConst
fnConsts trm = case trm of
  TVar _ -> Set.empty
  TObj _ -> Set.empty
  TFn fnConst args -> Set.unions $ Set.singleton fnConst : (fnConsts <$> args)