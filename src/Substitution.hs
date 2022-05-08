module Substitution
  ( applyToClause,
    applyToTerm,
    defined,
    empty,
    fromList,
    insert,
    lookup,
    map,
    singleton,
    zip,
    Sub (..),
  )
where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Syntax.Clause (Clause)
import Syntax.Literal (Literal (..))
import Syntax.Term (Term (..))
import Syntax.Variable (Var (..))
import Prelude hiding (lookup, map, zip)

-- | A finite mapping from variables to terms.
newtype Sub = Sub {unSub :: Map Var Term}
  deriving (Eq)

instance Show Sub where
  show (Sub sub) = "{\n" ++ unlines lines ++ "}"
    where
      lines = (\(var, trm) -> "  " ++ show var ++ " |-> " ++ show trm) <$> Map.toAscList sub

-- | Apply the given substitution to the term.
applyToTerm :: Sub -> Term -> Term
applyToTerm sub trm = case trm of
  TVar var -> Data.Maybe.fromMaybe trm $ Map.lookup var (unSub sub)
  TObj _ -> trm
  TFn fnConst args -> TFn fnConst (applyToTerm sub <$> args)

-- | Apply the given substitution to the clause.
applyToClause :: Sub -> Clause -> Clause
applyToClause sub clause = applyToLiteral sub `Set.map` clause

-- | Apply the given substitution to the literal.
applyToLiteral :: Sub -> Literal -> Literal
applyToLiteral sub literal = case literal of
  LPos rltnConst args -> LPos rltnConst (applyToTerm sub <$> args)
  LNeg rltnConst args -> LNeg rltnConst (applyToTerm sub <$> args)

-- | Return true iff the variable is defined in the substitution.
defined :: Var -> Sub -> Bool
defined var sub = isJust $ lookup var sub

empty :: Sub
empty = Sub Map.empty

fromList :: [(Var, Term)] -> Sub
fromList = Sub . Map.fromList

insert :: Var -> Term -> Sub -> Sub
insert var trm (Sub sub) = Sub $ Map.insert var trm sub

-- | Look up the term associated with the given variable.
lookup :: Var -> Sub -> Maybe Term
lookup var (Sub sub) = var `Map.lookup` sub

singleton :: Var -> Term -> Sub
singleton var trm = Sub $ Map.singleton var trm

map :: (Term -> Term) -> Sub -> Sub
map f (Sub sub) = Sub $ Map.map f sub

zip :: [Var] -> [Term] -> Sub
zip vars trms = Sub $ Map.fromAscList $ List.zip vars trms
