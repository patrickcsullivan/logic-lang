module Substitution (empty, singleton, insert, zip, applyToTerm, Substitution (..)) where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Syntax.Term (Term (..))
import Syntax.Variable (Var (..))
import Prelude hiding (zip)

-- | A finite mapping from variables to terms.
newtype Substitution = Substitution {unSubstitution :: Map Var Term}

empty :: Substitution
empty = Substitution Map.empty

singleton :: Var -> Term -> Substitution
singleton var trm = Substitution $ Map.singleton var trm

insert :: Var -> Term -> Substitution -> Substitution
insert var trm (Substitution sub) = Substitution $ Map.insert var trm sub

zip :: [Var] -> [Term] -> Substitution
zip vars trms = Substitution $ Map.fromAscList $ List.zip vars trms

-- | Apply the given substitution to the term.
applyToTerm :: Substitution -> Term -> Term
applyToTerm sub trm = case trm of
  TVar var -> Data.Maybe.fromMaybe trm $ Map.lookup var (unSubstitution sub)
  TObj _ -> trm
  TFn fnConst args -> TFn fnConst (applyToTerm sub <$> args)
