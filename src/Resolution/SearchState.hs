module Resolution.SearchState
  ( SearchState (..),
    initState,
  )
where

import Data.Sequence (Seq (Empty))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Resolution.Proof (IndexedProof (..))
import Syntax.Clause (Clause)

data SearchState = SearchState
  { used :: Seq (Int, Clause, IndexedProof),
    unused :: Seq (Int, Clause, IndexedProof)
  }

initState :: Set Clause -> SearchState
initState clauses =
  let premises = Seq.mapWithIndex (\i c -> (i, c, IPremise)) $ Seq.fromList $ Set.toAscList clauses
   in SearchState Empty premises