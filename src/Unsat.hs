module Unsat where

import Data.Map as Map
import Ground (groundTermPermutations)

-- type Substitution = Map Var Term

herbLoop mfn tfn fo0 objs fns freeVars n fo tried perms = do
  putStrLn $ show (length tried) ++ " ground instances tried; " ++ show (length fo) ++ " items in list"
  case perms of
    [] ->
      let perms' = groundTermPermutations objs fns (length freeVars) n
       in herbLoop mfn tfn fo0 objs fns freeVars (n + 1) fo tried perms'
    p : ps ->
      let fo' = mfn fo0 undefined fo
       in do
            if not (tfn fo')
              then return (p : tried)
              else herbLoop mfn tfn fo0 objs fns freeVars n fo' (p : tried) perms