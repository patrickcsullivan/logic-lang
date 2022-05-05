module Resolution where

import Data.Sequence (Seq (Empty, (:<|)), (><), (|>))
import qualified Data.Sequence as Seq
import qualified Substitution as Sub
import Syntax.Clausal (Literal (..), vars)
import Syntax.Variable (Var (..))

resolutionLoop :: Seq [Literal] -> Seq [Literal] -> Maybe ()
resolutionLoop used unused = case unused of
  Empty -> Nothing
  c :<| cs ->
    let used' = used |> c
        news = resolveClauses c =<< used
     in case Seq.elemIndexL [] cs of
          Just _ -> Just ()
          Nothing -> resolutionLoop used' (cs >< news)

resolveClauses :: [Literal] -> [Literal] -> Seq [Literal]
resolveClauses c1 c2 = undefined

renameVars :: String -> [Literal] -> [Literal]
renameVars prefix clause =
  let vs = vars clause
      vs' = (\(Var name) -> Var $ prefix ++ name) <$> vs
      sub = Sub.zip vs vs'
   in Sub.applyToClause sub clause