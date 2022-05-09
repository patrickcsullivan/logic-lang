module Syntax.Formula
  ( Formula (..),
    freeVars,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Syntax.Constant (RltnConst)
import Syntax.Term (Term)
import qualified Syntax.Term as Term
import Syntax.Variable (Var)

-- | A propositional formula.
data Formula
  = -- | False.
    FFalse
  | -- | True.
    FTrue
  | -- | An atomic formula that contains no propositional connectives. An atomic
    -- formula is a relation constant applied to terms.
    FAtom RltnConst [Term]
  | -- | Negation.
    FNot Formula
  | -- | Conjunction.
    FAnd Formula Formula
  | -- | Disjunction.
    FOr Formula Formula
  | -- | Implication.
    FImp Formula Formula
  | -- | Equivalence.
    FIff Formula Formula
  | -- | Univerally quantified propositional formula. Takes a variable binding and a scope.
    FForAll Var Formula
  | -- | Existentially quantified propositional formula. Takes a variable binding and a scope.
    FExists Var Formula
  deriving (Eq, Ord, Show)

-- | Return the set of free variables in the formula.
freeVars :: Formula -> Set Var
freeVars frm =
  case frm of
    FFalse -> Set.empty
    FTrue -> Set.empty
    FAtom _ args -> Set.unions (Term.vars <$> args)
    FNot p -> freeVars p
    FAnd p q -> freeVars p `Set.union` freeVars q
    FOr p q -> freeVars p `Set.union` freeVars q
    FImp p q -> freeVars p `Set.union` freeVars q
    FIff p q -> freeVars p `Set.union` freeVars q
    FForAll x p -> Set.delete x (freeVars p)
    FExists x p -> Set.delete x (freeVars p)