module Resolution where

import Data.Sequence (Seq (Empty, (:<|)), (><), (|>))
import qualified Data.Sequence as Seq
import qualified Substitution as Sub
import Syntax.Clause (Clause)
import Syntax.Variable (Var (..))

resolutionLoop :: Seq Clause -> Seq Clause -> Maybe ()
resolutionLoop used unused = case unused of
  Empty -> Nothing
  c :<| cs ->
    let used' = used |> c
        news = resolveClauses c =<< used
     in case Seq.elemIndexL [] cs of
          Just _ -> Just ()
          Nothing -> resolutionLoop used' (cs >< news)

resolveClauses :: Clause -> Clause -> Seq Clause
resolveClauses c1 c2 = undefined

-- renameVars :: String -> Clause -> Clause
-- renameVars prefix clause =
--   let vs = vars clause
--       vs' = (\(Var name) -> Var $ prefix ++ name) <$> vs
--       sub = Sub.zip vs vs'
--    in Sub.applyToClause sub clause