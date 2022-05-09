module Unification
  ( literalsMgu,
    solve,
    unifyAllTerms, -- Only exported for testing. TODO: Remove export and test something else.
    unifiable,
  )
where

import Control.Monad.Loops (anyM)
import Data.Maybe (isJust)
import Substitution (Sub)
import qualified Substitution as Sub
import Syntax.Constant (FnConst (..), ObjConst (..), RltnConst)
import Syntax.Literal (Literal (..))
import Syntax.Term (Term (..))
import Syntax.Variable (Var)

-- unify :: [(Term, Term)] -> Maybe Sub
-- unify eqtns = solve <$> unifyAllTerms Sub.empty eqtns

-- | Find the most general unifier for the set of literals.
literalsMgu :: [Literal] -> Maybe Sub
literalsMgu = mguWithEnv Sub.empty

mguWithEnv :: Sub -> [Literal] -> Maybe Sub
mguWithEnv env lits = case lits of
  a : b : rest -> do
    env' <- unifyLiterals env a b
    mguWithEnv env' (b : rest)
  _ -> Just $ solve env

solve :: Sub -> Sub
solve env =
  let env' = Sub.map (Sub.applyToTerm env) env
   in if env' == env
        then env
        else solve env'

unifiable :: Literal -> Literal -> Bool
unifiable l1 l2 = isJust $ unifyLiterals Sub.empty l1 l2

unifyLiterals :: Sub -> Literal -> Literal -> Maybe Sub
unifyLiterals env l1 l2 = case (l1, l2) of
  (LPos _ _, LNeg _ _) -> Nothing
  (LNeg _ _, LPos _ _) -> Nothing
  (LPos rltnConst1 args1, LPos rltnConst2 args2) -> unifyRelations env (rltnConst1, args1) (rltnConst2, args2)
  (LNeg rltnConst1 args1, LNeg rltnConst2 args2) -> unifyRelations env (rltnConst1, args1) (rltnConst2, args2)

unifyRelations :: Sub -> (RltnConst, [Term]) -> (RltnConst, [Term]) -> Maybe Sub
unifyRelations env (rltnConst1, args1) (rltnConst2, args2) =
  if rltnConst1 == rltnConst2 && length args1 == length args2
    then unifyAllTerms env (zip args1 args2)
    else Nothing

unifyAllTerms :: Sub -> [(Term, Term)] -> Maybe Sub
unifyAllTerms env eqtns = case eqtns of
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
        then unifyAllTerms env (zip args1 args2 ++ es)
        else Nothing
    (TVar var, trm) ->
      case var `Sub.lookup` env of
        Just binding -> unifyAllTerms env ((binding, trm) : es)
        Nothing ->
          case isTrivial env var trm of
            Just True -> unifyAllTerms env es
            Just False -> unifyAllTerms (Sub.insert var trm env) es
            Nothing -> Nothing -- There was a cycle.
    (trm, TVar var) -> unifyAllTerms env ((TVar var, trm) : es)

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