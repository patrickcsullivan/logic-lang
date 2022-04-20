module Ast where

newtype Var = Var
  { varName :: String
  }
  deriving (Eq, Ord, Show)

data RltnConst = RltnConst
  { rltnConstName :: String,
    rltnConstArity :: Int
  }
  deriving (Eq, Ord, Show)

data FnConst = FnConst
  { fnConstName :: String,
    fnConstArity :: Int
  }
  deriving (Eq, Ord, Show)

newtype ObjConst = ObjConst
  { objConstName :: String
  }
  deriving (Eq, Ord, Show)

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

-- | A first-order term which is intended to denote an "object" in the domain
-- being reasoned about. A term can be built up from "object"-denoting variables
-- and constants using functions.
data Term
  = TVar Var
  | TObj ObjConst
  | TFn FnConst [Term]
  deriving (Eq, Ord, Show)