module Sub (empty, singleton, insert, zip, applyToTerm, Sub (..)) where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Syntax.Term (Term (..))
import Syntax.Variable (Var (..))
import Prelude hiding (zip)

newtype Sub = Sub {unSub :: Map Var Term}

empty :: Sub
empty = Sub Map.empty

singleton :: Var -> Term -> Sub
singleton var trm = Sub $ Map.singleton var trm

insert :: Var -> Term -> Sub -> Sub
insert var trm (Sub sub) = Sub $ Map.insert var trm sub

zip :: [Var] -> [Term] -> Sub
zip vars trms = Sub $ Map.fromAscList $ List.zip vars trms

-- | Apply the given substitution to the term.
applyToTerm :: Sub -> Term -> Term
applyToTerm sub trm = case trm of
  TVar var -> Data.Maybe.fromMaybe trm $ Map.lookup var (unSub sub)
  TObj _ -> trm
  TFn fnConst args -> TFn fnConst (applyToTerm sub <$> args)
