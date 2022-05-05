module Unification
  ( unify,
  )
where

import Control.Monad.Loops (anyM)
import Substitution (Sub)
import qualified Substitution as Sub
import Syntax.Constant (FnConst (..), ObjConst (..))
import Syntax.Term (Term (..))
import Syntax.Variable (Var)

unify :: [(Term, Term)] -> Maybe Sub
unify eqtns = solve <$> unifyAll Sub.empty eqtns

solve :: Sub -> Sub
solve env =
  let env' = Sub.map (Sub.applyToTerm env) env
   in if env' == env
        then env
        else solve env'

unifyAll :: Sub -> [(Term, Term)] -> Maybe Sub
unifyAll env eqtns = case eqtns of
  [] -> Just env
  e : es -> case e of
    (TObj _, TFn _ _) -> Nothing
    (TFn _ _, TObj _) -> Nothing
    (TObj oc1, TObj oc2) ->
      if oc1 == oc2
        then Just env
        else Nothing
    (TFn fc1 args1, TFn fc2 args2) ->
      if fc1 == fc2
        then unifyAll env (zip args1 args2 ++ es)
        else Nothing
    (TVar var, trm) ->
      case var `Sub.lookup` env of
        Just binding -> unifyAll env ((binding, trm) : es)
        Nothing ->
          case isTrivial env var trm of
            Just True -> unifyAll env es
            Just False -> unifyAll (Sub.insert var trm env) es
            Nothing -> Nothing -- There was a cycle.
    (trm, TVar var) -> unifyAll env ((TVar var, trm) : es)

-- | Check whether unification is possible and trivial, possible and
-- non-trivial, or impossible due to a cycle.
isTrivial :: Sub -> Var -> Term -> Maybe Bool
isTrivial env x trm = case trm of
  TVar y ->
    if x == y
      then -- x and y are the same variable, so the unification is trivial
        Just True
      else case Sub.lookup y env of
        -- x is unifying with another variable y, that's also not in the envirnoment
        Nothing -> Just False
        -- x is unifying with y, which is mapped to some term, so check that x
        -- is unifiable with the term
        Just yTrm -> isTrivial env x yTrm
  TObj objConst -> Just False
  TFn fnConst args ->
    case anyM (isTrivial env x) args of
      -- A cycle was detected in one of the arguments.
      Nothing -> Nothing
      -- Unification with all arguments is non-trivial.
      Just False -> Just False
      -- One of the arguments contains a trivial unification with x, meaning
      -- that there is a cycle. (We have the situation where we're trying to
      -- unify X with f(X).)
      Just True -> Nothing