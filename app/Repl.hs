module Repl
  ( repl,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Set (Set)
import qualified Data.Set as Set
import Parser (formulaReplP)
import Resolution (SearchState (..), unsatisfiable)
import Resolution.Proof (IndexedProof)
import qualified Resolution.Proof as Proof
import Scanner (spaceConsumer)
import Syntax.Clause (Clause)
import qualified Syntax.Clause as Clause
import qualified Syntax.Clause.Transform as Clause
import Syntax.Formula (Formula (..))
import qualified Syntax.Formula as Formula
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    outputStr,
    outputStrLn,
    runInputT,
  )
import Text.Megaparsec (errorBundlePretty, parse)

data ReplState = New | ProofNotFound | ProofFound SearchState Clause IndexedProof

repl :: [Formula] -> IO ()
repl fileFormulas = runInputT defaultSettings (mainLoop fileClauses New)
  where
    fileClauses = Set.unions (Clause.fromFormula <$> fileFormulas)

mainLoop :: Set Clause -> ReplState -> InputT IO ()
mainLoop fileClauses replState = do
  maybeInput <- getInputLine "?> "
  case maybeInput of
    Nothing -> mainLoop fileClauses replState
    Just "" -> mainLoop fileClauses replState -- TODO: Handle all whitespace.
    Just (':' : rest) ->
      case parseCommand rest of
        Just Quit -> outputStrLn "Quitting."
        Just ShowProof ->
          case replState of
            New -> do
              outputStrLn "You haven't tried to prove anything yet."
              mainLoop fileClauses replState
            ProofNotFound -> do
              outputStrLn "No proof found for entailment of the last formula."
              mainLoop fileClauses replState
            ProofFound (SearchState used _) goal proof -> do
              let usedWithoutIndexes = (\(i, c, p) -> (c, p)) <$> used
              liftIO (Proof.prettyPrint usedWithoutIndexes (goal, proof))
              mainLoop fileClauses replState
        Nothing -> do
          outputStrLn "Unrecognized command."
          mainLoop fileClauses replState
    Just input ->
      case parse formulaReplP "repl" input of
        Left err -> outputStr (errorBundlePretty err) >> mainLoop fileClauses replState
        Right fo ->
          if Set.null $ Formula.freeVars fo
            then
              let negated = Clause.fromFormula (FNot fo)
               in case unsatisfiable (fileClauses `Set.union` negated) of
                    (_, []) -> do
                      outputStrLn "No proof of entailment found."
                      mainLoop fileClauses ProofNotFound
                    (searchState, (goal, proof) : _) -> do
                      outputStrLn "Entailed."
                      mainLoop fileClauses (ProofFound searchState goal proof)
            else do
              outputStr "Formula cannot contain free variables."
              mainLoop fileClauses replState

-- outputStr (show fo) >> mainLoop

data Command = Quit | ShowProof

parseCommand :: String -> Maybe Command
parseCommand s = case s of
  "q" -> Just Quit
  "quit" -> Just Quit
  "p" -> Just ShowProof
  "proof" -> Just ShowProof
  _ -> Nothing

process :: String -> IO ()
process input = do
  case parse formulaReplP "repl" input of
    Left err -> putStr $ errorBundlePretty err
    Right fo -> print fo
