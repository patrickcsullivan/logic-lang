module Syntax.Formula
  ( Formula (..),
  )
where

import Syntax.Constant (RltnConst)
import Syntax.Term (Term)
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
