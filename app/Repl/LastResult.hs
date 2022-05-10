module Repl.LastResult
  ( LastResult (..),
  )
where

import Resolution.Proof (IndexedProof (..))
import Resolution.SearchState (SearchState (..))
import Syntax.Clause (Clause)
import Syntax.Variable (Var)

data LastResult
  = NoHistory
  | EntailmentCheck
      { searchState :: SearchState,
        answer :: Maybe (Clause, IndexedProof)
      }
  | AnswerExtraction
      { vars :: [Var],
        terminationCheck :: Clause -> Bool,
        searchState :: SearchState,
        shownAnswers :: [(Clause, IndexedProof)],
        unshownAnswers :: [(Clause, IndexedProof)]
      }