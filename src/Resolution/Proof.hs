module Resolution.Proof
  ( IndexedProof (..),
    prettyPrint,
  )
where

import Data.List (intercalate)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax.Clause (Clause)

data IndexedProof = IPremise | IResolvent Int Int

prettyPrint :: Seq (Clause, IndexedProof) -> (Clause, IndexedProof) -> IO ()
prettyPrint inferred (conclClause, conclProof) =
  let indexes = Set.toAscList $ usedIndexes inferred conclProof
      steps =
        ( \i ->
            let (c, prf) = Seq.index inferred i
             in (i, c, prf)
        )
          <$> indexes
      proofs = conclProof : ((\(_, _, p) -> p) <$> steps)
      iColWidth = indexColWidth indexes
      pColWidth = proofColWidth proofs
      lines = (prettyStep iColWidth pColWidth <$> steps) ++ [prettyConcl iColWidth pColWidth (conclClause, conclProof)]
   in do
        putStrLn ""
        sequence_ $ putStrLn <$> lines

prettyStep :: Int -> Int -> (Int, Clause, IndexedProof) -> String
prettyStep iColWidth pColWidth (index, clause, proof) =
  let iCol = padRightSpaces iColWidth $ show index
      pCol = padRightSpaces pColWidth $ prettyProof proof
      cCol = prettyClause clause
   in iCol ++ " | " ++ pCol ++ " | " ++ cCol

prettyConcl :: Int -> Int -> (Clause, IndexedProof) -> String
prettyConcl iColWidth pColWidth (clause, proof) =
  let iCol = padRightSpaces iColWidth "Goal"
      pCol = padRightSpaces pColWidth $ prettyProof proof
      cCol = prettyClause clause
   in iCol ++ " | " ++ pCol ++ " | " ++ cCol

indexColWidth :: [Int] -> Int
indexColWidth indexes =
  let entries = "Goal" : (show <$> indexes)
   in maximum $ length <$> entries

proofColWidth :: [IndexedProof] -> Int
proofColWidth proofs =
  let entries = prettyProof <$> proofs
   in maximum $ length <$> entries

prettyClause :: Clause -> String
prettyClause clause = "{ " ++ intercalate ", " (show <$> lits) ++ " }"
  where
    lits = Set.toAscList clause

prettyProof :: IndexedProof -> String
prettyProof proof = case proof of
  IPremise -> "Premise"
  IResolvent i1 i2 -> show i1 ++ ", " ++ show i2

usedIndexes :: Seq (Clause, IndexedProof) -> IndexedProof -> Set Int
usedIndexes inferred conclProof = case conclProof of
  IPremise -> Set.empty
  IResolvent i1 i2 ->
    let (_, premProof1) = inferred `Seq.index` i1
        (_, premProof2) = inferred `Seq.index` i2
     in Set.unions
          [ Set.singleton i1,
            Set.singleton i2,
            usedIndexes inferred premProof1,
            usedIndexes inferred premProof2
          ]

padRightSpaces :: Int -> String -> String
padRightSpaces totalWidth s =
  let padWidth = max 0 $ totalWidth - length s
   in s ++ replicate padWidth ' '