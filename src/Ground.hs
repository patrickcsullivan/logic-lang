module Ground
  ( groundTerms,
    groundTermPermutations,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Syntax.Constant (FnConst (..), ObjConst (..), RltnConst (..))
import Syntax.Term (Term (..))
import Syntax.Variable (Var (..))

-- | Generate all ground terms involving "depth-`n`" functions.
groundTerms :: [ObjConst] -> [FnConst] -> Int -> [Term]
groundTerms objs fns n =
  if n == 0
    then TObj <$> objs
    else do
      (FnConst name arity) <- fns
      let argCombinations = groundTermPermutations objs fns arity (n - 1)
      TFn (FnConst name arity) <$> argCombinations

-- | Generate all `m`-length permunations of ground terms, where each term has a
-- "depth" of `n`.
groundTermPermutations :: [ObjConst] -> [FnConst] -> Int -> Int -> [[Term]]
-- A nullary function can have exactly one combination of depth-0 arguments, the
-- empty list of arguments.
groundTermPermutations objs fns 0 0 = [[]]
-- A nullary function has no combinations of depth-n arguments when n > 0.
groundTermPermutations objs fns 0 n = []
groundTermPermutations objs fns arity n = do
  -- for each depth from 0 to n
  depth <- [0 .. n]
  -- for each term with the given depth
  head <- groundTerms objs fns depth
  -- for each combination of (arity - 1) args where each arg has a max depth of
  -- (n - depth)
  tail <- groundTermPermutations objs fns (arity - 1) (n - depth)
  return $ head : tail

exp1 =
  [ [TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TObj (ObjConst {objConstName = "c"})]],
    [TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TObj (ObjConst {objConstName = "d"})]]
  ]

exp2 =
  [ [TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TObj (ObjConst {objConstName = "c"})]]],
    [TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TObj (ObjConst {objConstName = "d"})]]]
  ]