module Syntax.Clause
  ( Clause,
    vars,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Syntax.Literal (Literal)
import qualified Syntax.Literal as Literal
import Syntax.Variable (Var)

-- | A disjunction of literals.
type Clause = [Literal]

-- | Return the set of variables in the clause.
vars :: Clause -> Set Var
vars cl = Set.unions (Literal.vars <$> cl)
