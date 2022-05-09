module Syntax.Literal
  ( Literal (..),
    fnConsts,
    negate,
    objConsts,
    vars,
  )
where

import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax.Constant (FnConst (..), ObjConst (..), RltnConst (..))
import Syntax.Term (Term (..))
import qualified Syntax.Term as Term
import Syntax.Variable (Var (..))
import Prelude hiding (negate)

-- | A relation or negated relation that is applied to term arguments.
data Literal
  = -- | Positive literal.
    LPos RltnConst [Term]
  | -- | Negative literal.
    LNeg RltnConst [Term]
  deriving (Eq, Ord)

instance Show Literal where
  show literal = case literal of
    LPos rltnConst args -> showRltn rltnConst args
    LNeg rltnConst args -> "~" ++ showRltn rltnConst args
    where
      showRltn (RltnConst rltnName _) args = rltnName ++ "(" ++ intercalate ", " (show <$> args) ++ ")"

-- | Return the set of function constants in the literal.
fnConsts :: Literal -> Set FnConst
fnConsts lit = case lit of
  LPos _ trms -> Set.unions (Term.fnConsts <$> trms)
  LNeg _ trms -> Set.unions (Term.fnConsts <$> trms)

-- | Negate the literal.
negate :: Literal -> Literal
negate lit = case lit of
  LPos rltnConst args -> LNeg rltnConst args
  LNeg rltnConst args -> LPos rltnConst args

-- | Return the set of object constants in the literal.
objConsts :: Literal -> Set ObjConst
objConsts lit = case lit of
  LPos _ trms -> Set.unions (Term.objConsts <$> trms)
  LNeg _ trms -> Set.unions (Term.objConsts <$> trms)

-- | Return the set of variables in the literal.
vars :: Literal -> Set Var
vars lit = case lit of
  LPos _ trms -> Set.unions (Term.vars <$> trms)
  LNeg _ trms -> Set.unions (Term.vars <$> trms)