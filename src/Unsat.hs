module Unsat where

import Data.Set (Set)
import qualified Data.Set as Set
import Ground (groundTermPermutations)
import Sub (Sub (..))
import qualified Sub
import Syntax.Clausal (Literal (..))
import qualified Syntax.Clausal as Clausal
import Syntax.Constant (FnConst, ObjConst)
import Syntax.Formula (Formula (..))
import Syntax.Term (Term (..))
import Syntax.Variable (Var)

gilmore fo = do
  let cnf = Clausal.fromFormula $ FNot fo
  let freeVars = clausalVars cnf
  let objConsts = clausalObjConsts cnf
  let fnConsts = clausalFnConsts cnf
  let objTrms = TObj `Set.map` objConsts
  undefined

-- rslt <- herbLoop gilmoreMfn gilmoreTfn objTrms

gilmoreMfn djs0 ifn djs = undefined

gilmoreTfn djs = djs /= []

herbLoop ::
  Foldable t1 =>
  -- | mfn: modification function that augments the ground instances with a new
  -- instance, whatever form they're stored in
  (t2 -> Sub -> t1 a -> t1 a) ->
  -- | tfn: satisfiability test
  (t1 a -> Bool) ->
  -- | f0:
  t2 ->
  -- | objs: the object constants in the formula
  [ObjConst] ->
  -- | fns: the function constants in the formula
  [FnConst] ->
  -- | freeVars: the free variables in the formula
  [Var] ->
  -- | n: next level of the enumeration to generate
  Int ->
  -- | fo:
  t1 a ->
  -- | tried: the set of permutations of ground instances that have been tried
  -- so far
  [[Term]] ->
  -- | perms: the remaining permutations of ground instances to try at the
  -- current level of the enumeration
  [[Term]] ->
  IO [[Term]]
herbLoop mfn tfn fo0 objs fns freeVars n fo tried perms = do
  putStrLn $ show (length tried) ++ " ground instances tried; " ++ show (length fo) ++ " items in list"
  case perms of
    [] ->
      let perms' = groundTermPermutations objs fns (length freeVars) n
       in herbLoop mfn tfn fo0 objs fns freeVars (n + 1) fo tried perms'
    p : ps ->
      let fo' = mfn fo0 (freeVars `Sub.zip` p) fo
       in do
            if not (tfn fo')
              then return (p : tried)
              else herbLoop mfn tfn fo0 objs fns freeVars n fo' (p : tried) perms

clausalVars :: [[Literal]] -> Set Var
clausalVars clauses = Set.unions (clauseVars <$> clauses)
  where
    clauseVars clause = Set.unions (Clausal.vars <$> clause)

clausalObjConsts :: [[Literal]] -> Set ObjConst
clausalObjConsts clauses = Set.unions (clauseObjConsts <$> clauses)
  where
    clauseObjConsts clause = Set.unions (Clausal.objConsts <$> clause)

clausalFnConsts :: [[Literal]] -> Set FnConst
clausalFnConsts clauses = Set.unions (clauseFnConsts <$> clauses)
  where
    clauseFnConsts clause = Set.unions (Clausal.fnConsts <$> clause)
