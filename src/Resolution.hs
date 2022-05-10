{-# LANGUAGE TupleSections #-}

module Resolution
  ( IndexedProof (..),
    resolutionLoop,
    unsatisfiable,
  )
where

import Data.List (subsequences)
import Data.Sequence (Seq (Empty, (:<|)), (><), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Resolution.Proof (IndexedProof (..))
import Resolution.SearchState (SearchState (..))
import qualified Resolution.SearchState as SearchState
import Substitution (Sub)
import qualified Substitution as Sub
import Syntax.Clause (Clause)
import qualified Syntax.Clause as Clause
import Syntax.Literal (Literal)
import qualified Syntax.Literal as Literal
import Syntax.Term (Term (..))
import Syntax.Variable (Var (..))
import Unification (literalsMgu, unifiable)

unsatisfiable :: Set Clause -> (SearchState, Maybe (Clause, IndexedProof))
unsatisfiable clauses =
  case resolutionLoop Set.null (SearchState.initState clauses) of
    (searchState, (clause, proof) : _) -> (searchState, Just (clause, proof))
    (searchState, []) -> (searchState, Nothing)

-- | Resolve the first "unused" clause with each "used" clause, generating new
-- "unused" clauses. Repeat until one of the "unsued" clauses passes the search
-- function or until all possible resolutions have been exhausted.
resolutionLoop :: (Clause -> Bool) -> SearchState -> (SearchState, [(Clause, IndexedProof)])
resolutionLoop f (SearchState used unused) = case unused of
  Empty -> (SearchState used unused, [])
  c :<| cs ->
    let used' = used |> c
        news = resolveClauses c =<< used'
        next = length used' + length cs
        newsWithIndex = Seq.mapWithIndex (\i (c, p) -> (i + next, c, p)) news
        unused' = cs >< newsWithIndex
        nextState = SearchState used' unused'
     in case Seq.findIndicesL (f . fst) news of
          [] -> resolutionLoop f nextState
          goalIndices ->
            let goals = Seq.index news <$> goalIndices
                state = SearchState used' unused'
             in (nextState, goals)

-- data Proof
--   = Premise Clause
--   | Resolvent Clause Proof Proof
--   deriving (Show)

-- buildProof :: Seq (Clause, IndexedProof) -> (Clause, IndexedProof) -> Proof
-- buildProof infered (goalClause, goalProof) = case goalProof of
--   IPremise -> Premise goalClause
--   IResolvent premiseIndex1 premiseIndex2 ->
--     let premiseProof1 = buildProof infered (infered `Seq.index` premiseIndex1)
--         premiseProof2 = buildProof infered (infered `Seq.index` premiseIndex2)
--      in Resolvent goalClause premiseProof1 premiseProof2

-- | Generate all possible resolvent clauses from the two premise clauses.
resolveClauses :: (Int, Clause, IndexedProof) -> (Int, Clause, IndexedProof) -> Seq (Clause, IndexedProof)
resolveClauses (index1, clause1, _) (index2, clause2, _) =
  let clause1' = renameVars "X" clause1
      clause2' = renameVars "Y" clause2
      rs = foldr (resolvents clause1' clause2') Empty clause1'
   in (,IResolvent index1 index2) <$> rs

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
  let ps2 = Set.filter (unifiable (Literal.negate p)) c2
   in if null ps2
        then Empty -- No complements of p, so no resolvents can be inferred.
        else
          let -- Pick all the literals in the first clause that are unifiable with
              -- p, other than p itself.
              ps1 = Set.filter (\q -> q /= p && unifiable p q) c1
              -- All possible pairs of subsets of literals from the first clause
              -- that are unifiable with p and include p and non-empty subsets
              -- of literals from the second clause.
              pairs = Seq.fromList $ do
                -- Each subset of literals in the first clause that are
                -- unifiable with p and that also include p.
                p1Subset <- (p :) <$> subsequences (Set.toAscList ps1)
                -- Each non-empty subset of liters in the second clause.
                p2Subset <- filter (not . null) $ subsequences $ Set.toAscList ps2
                return (Set.fromList p1Subset, Set.fromList p2Subset)
           in foldr
                ( \(p1Subset, p2Subset) soFar ->
                    case resolvent c1 p1Subset c2 p2Subset of
                      Nothing -> soFar
                      Just r -> r :<| soFar
                )
                acc
                pairs

resolvent :: Set Literal -> Set Literal -> Set Literal -> Set Literal -> Maybe Clause
resolvent a aSub b bSub = do
  let negBSub = Set.map Literal.negate bSub
  mgu <- literalsMgu $ Set.toList $ Set.union aSub (Set.map Literal.negate bSub)
  let aMinusASub = a `Set.difference` aSub
  let bMinusBSub = b `Set.difference` bSub
  return $ mgu `Sub.applyToClause` (aMinusASub `Set.union` bMinusBSub)

-- | Append the given prefix to each of the variable names in the clause.
renameVars :: String -> Clause -> Clause
renameVars prefix clause =
  let vs = Set.toList $ Clause.vars clause
      vs' = (\(Var name) -> TVar $ Var $ prefix ++ name) <$> vs
      sub = Sub.zip vs vs'
   in Sub.applyToClause sub clause
