module Unification where

import Substitution (Substitution)
import qualified Substitution
import Syntax.Constant (FnConst (..), ObjConst (..))
import Syntax.Term (Term (..))
import Syntax.Variable (Var)

data TrivialRslt = Trivial | Cyclic | NotTrivialOrCyclic

isTrivial :: Substitution -> Var -> Term -> TrivialRslt
isTrivial = undefined

unify :: Substitution -> Term -> Term -> Maybe Substitution
unify env trm1 trm2 = case (trm1, trm2) of
  (TObj _, TFn _ _) -> Nothing
  (TFn _ _, TObj _) -> Nothing
  (TObj )

unifyAll :: Substitution -> [(Term, Term)] -> Maybe Substitution
unifyAll env eqtns = case eqtns of
  [] -> Just env
  x0 : x1 -> _

-- isTrivial env x trm = case trm of
--   TVar y ->
--     if x == y
--       then Trivial
--       else undefined
--   TObj objConst -> NotTrivialOrCyclic
--   TFn fnConst args ->
--     if any (\ arg -> isTrivial env x arg == Trivial)
--       then Cyclic
--       else
