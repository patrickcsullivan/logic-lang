module Ground
  ( groundTerms,
  )
where

import Ast (FnConst (..), Formula (..), ObjConst (..), RltnConst (..), Term (..), Var (..))
import Data.Set (Set)
import qualified Data.Set as Set

-- | Generate all ground terms involving "depth-`n`" functions.
groundTerms :: [ObjConst] -> [FnConst] -> Int -> [Term]
groundTerms objs fns n =
  if n == 0
    then TObj <$> objs
    else do
      (FnConst name arity) <- fns
      let argCombinations = groundArgCombinations objs fns arity (n - 1)
      TFn (FnConst name arity) <$> argCombinations

-- | Generate all combinations of arguments for a function with the given arity
-- where each argument has a maximum "depth" of `n`.
groundArgCombinations :: [ObjConst] -> [FnConst] -> Int -> Int -> [[Term]]
-- A nullary function can have exactly one combination of depth-0 arguments, the
-- empty list of arguments.
groundArgCombinations objs fns 0 0 = [[]]
-- A nullary function has no combinations of depth-n arguments when n > 0.
groundArgCombinations objs fns 0 n = []
groundArgCombinations objs fns arity n = do
  -- for each depth from 0 to n
  depth <- [0 .. n]
  -- for each term with the given depth
  head <- groundTerms objs fns depth
  -- for each combination of (arity - 1) args where each arg has a max depth of
  -- (n - depth)
  tail <- groundArgCombinations objs fns (arity - 1) (n - depth)
  return $ head : tail
