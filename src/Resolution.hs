module Resolution where

import Data.List (subsequences)
import Data.Sequence (Seq (Empty, (:<|)), (><), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Substitution as Sub
import Syntax.Clause (Clause)
import qualified Syntax.Clause as Clause
import Syntax.Literal (Literal)
import qualified Syntax.Literal as Literal
import Syntax.Term (Term (..))
import Syntax.Variable (Var (..))
import Unification (unifiable)

-- | Resolve the first "unused" clause with each "used" clause, generating new
-- "unused" clauses. Repeat until one of the "unsued" clauses is empty,
-- indicating unsatisfiability, or until all possible resolutions have been
-- exhausted.
unsatLoop :: Seq Clause -> Seq Clause -> Maybe ()
unsatLoop used unused = case unused of
  Empty -> Nothing
  c :<| cs ->
    case Seq.elemIndexL [] cs of
      Just _ -> Just ()
      Nothing ->
        let used' = used |> c
            news = resolveClauses c =<< used'
         in unsatLoop used' (cs >< news)

-- | Generate all possible resolvent clauses from the two premise clauses.
resolveClauses :: Clause -> Clause -> Seq Clause
resolveClauses c1 c2 =
  let c1' = renameVars "X" c1
      c2' = renameVars "Y" c2
   in foldr (resolvents c1' c2') Empty c1'

-- | Return all resolvents that can be inferred by unifying the given literal
-- from the first clause with complementary literals in the second clause.
resolvents ::
  -- First premise clause.
  Clause ->
  -- Second premise clause.
  Clause ->
  -- A literal from the first premise clause.
  Literal ->
  -- Resolvent clauses that have been generated so far.
  Seq Clause ->
  Seq Clause
resolvents c1 c2 p acc =
  -- Pick all the literals in the second clause that are complements of p.
  let ps2 = filter (unifiable (Literal.negate p)) c2
   in if null ps2
        then Empty -- No complements of p, so no resolvents can be inferred.
        else
          let -- Pick all the literals in the first clause that are unifiable with
              -- p, other than p itself.
              ps1 = filter (\q -> q /= p && unifiable p q) c1
              -- All possible pairs of subsets of literals from the first clause
              -- that are unifiable with p and include p and non-empty subsets
              -- of literals from the second clause.
              pairs = do
                -- Each subset of literals in the first clause that are
                -- unifiable with p and that also include p.
                p1Subset <- (p :) <$> subsequences ps1
                -- Each non-empty subset of liters in the second clause.
                p2Subset <- filter (not . null) $ subsequences ps2
                return (p1Subset, p2Subset)
           in foldr
                ( \(p1Subset, p2Subset) soFar ->
                    undefined
                )
                acc
                pairs

-- | Append the given prefix to each of the variable names in the clause.
renameVars :: String -> Clause -> Clause
renameVars prefix clause =
  let vs = Set.toList $ Clause.vars clause
      vs' = (\(Var name) -> TVar $ Var $ prefix ++ name) <$> vs
      sub = Sub.zip vs vs'
   in Sub.applyToClause sub clause
