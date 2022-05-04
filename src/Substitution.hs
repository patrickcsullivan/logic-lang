module Substitution
  ( applyToTerm,
    defined,
    empty,
    fromList,
    insert,
    lookup,
    map,
    singleton,
    zip,
    Substitution (..),
  )
where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Syntax.Term (Term (..))
import Syntax.Variable (Var (..))
import Prelude hiding (lookup, map, zip)

-- | A finite mapping from variables to terms.
newtype Substitution = Substitution {unSubstitution :: Map Var Term}
  deriving (Eq)

instance Show Substitution where
  show (Substitution sub) = "{\n" ++ unlines lines ++ "}"
    where
      lines = (\(var, trm) -> "  " ++ show var ++ " |-> " ++ show trm) <$> Map.toAscList sub

-- | Apply the given substitution to the term.
applyToTerm :: Substitution -> Term -> Term
applyToTerm sub trm = case trm of
  TVar var -> Data.Maybe.fromMaybe trm $ Map.lookup var (unSubstitution sub)
  TObj _ -> trm
  TFn fnConst args -> TFn fnConst (applyToTerm sub <$> args)

-- | Return true iff the variable is defined in the substitution.
defined :: Var -> Substitution -> Bool
defined var sub = isJust $ lookup var sub

empty :: Substitution
empty = Substitution Map.empty

fromList :: [(Var, Term)] -> Substitution
fromList = Substitution . Map.fromList

insert :: Var -> Term -> Substitution -> Substitution
insert var trm (Substitution sub) = Substitution $ Map.insert var trm sub

-- | Look up the term associated with the given variable.
lookup :: Var -> Substitution -> Maybe Term
lookup var (Substitution sub) = var `Map.lookup` sub

singleton :: Var -> Term -> Substitution
singleton var trm = Substitution $ Map.singleton var trm

map :: (Term -> Term) -> Substitution -> Substitution
map f (Substitution sub) = Substitution $ Map.map f sub

zip :: [Var] -> [Term] -> Substitution
zip vars trms = Substitution $ Map.fromAscList $ List.zip vars trms
