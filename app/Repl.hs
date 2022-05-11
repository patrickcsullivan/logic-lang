module Repl
  ( repl,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.Set (Set)
import qualified Data.Set as Set
import Parser (formulaReplP)
import Repl.Command (Command (..))
import qualified Repl.Command as Command
import Repl.LastResult (LastResult (..))
import Resolution (resolutionLoop, unsatisfiable)
import Resolution.Proof (IndexedProof, prettyPrintAll)
import qualified Resolution.Proof as Proof
import Resolution.SearchState (SearchState (..))
import qualified Resolution.SearchState as SearchState
import Syntax.Clause (Clause)
import qualified Syntax.Clause as Clause
import qualified Syntax.Clause.Transform as Clause
import Syntax.Constant (RltnConst (..))
import Syntax.Formula (Formula (..))
import qualified Syntax.Formula as Formula
import Syntax.Literal (Literal (..))
import Syntax.Term (Term (..))
import Syntax.Variable (Var)
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    outputStr,
    outputStrLn,
    runInputT,
  )
import Text.Megaparsec (errorBundlePretty, parse)

repl :: [Formula] -> IO ()
repl fileFormulas = runInputT defaultSettings (mainLoop fileClauses NoHistory)
  where
    fileClauses = Set.unions (Clause.fromFormula <$> fileFormulas)

mainLoop :: Set Clause -> LastResult -> InputT IO ()
mainLoop fileClauses last = do
  maybeInput <- getInputLine "?> "
  case maybeInput of
    Nothing -> mainLoop fileClauses last
    Just (':' : rest) -> case Command.parse rest of
      Nothing -> do
        outputStrLn "Unrecognized command."
        mainLoop fileClauses last
      Just cmd -> handleCommand fileClauses last cmd
    Just input | all isSpace input -> mainLoop fileClauses last
    Just input -> case parse formulaReplP "repl" input of
      Left err -> do
        outputStr (errorBundlePretty err)
        mainLoop fileClauses last
      Right fo -> handleFormula fileClauses fo

handleCommand :: Set Clause -> LastResult -> Command -> InputT IO ()
handleCommand fileClauses last command = case command of
  Quit -> outputStrLn "Quitting."
  LastProof ->
    case last of
      NoHistory -> do
        outputStrLn "There is no proof to show."
        mainLoop fileClauses last
      EntailmentCheck _ Nothing -> do
        outputStrLn "There is no proof to show."
        mainLoop fileClauses last
      EntailmentCheck (SearchState usedClauses _) (Just (goal, proof)) -> do
        let usedClauses' = (\(i, c, p) -> (c, p)) <$> usedClauses
        liftIO (Proof.prettyPrint usedClauses' (goal, proof))
        mainLoop fileClauses last
      AnswerExtraction _ _ (SearchState usedClauses _) ((answer, proof) : _) _ -> do
        let usedClauses' = (\(i, c, p) -> (c, p)) <$> usedClauses
        liftIO (Proof.prettyPrint usedClauses' (answer, proof))
        mainLoop fileClauses last
      AnswerExtraction _ _ _ [] _ -> do
        -- This case will only occur if there were no answers at all.
        outputStrLn "There is no proof to show."
        mainLoop fileClauses last
  LastAnswer ->
    case last of
      NoHistory -> do
        outputStrLn "No answer to show."
        mainLoop fileClauses last
      EntailmentCheck _ (Just _) -> do
        outputStrLn "Entailed."
        mainLoop fileClauses last
      EntailmentCheck _ Nothing -> do
        outputStrLn "No proof of entailment found."
        mainLoop fileClauses last
      AnswerExtraction vars _ _ ((answer, _) : _) _ -> do
        outputStr $ showAnswer vars answer
        mainLoop fileClauses last
      AnswerExtraction _ _ _ [] _ -> do
        -- This case will only occur if there were no answers at all.
        outputStrLn "No answers found."
        mainLoop fileClauses last
  NextAnswer ->
    case last of
      AnswerExtraction vars terminationCheck searchState shownAnswers unshownAnswers -> do
        findAnswers fileClauses vars terminationCheck searchState shownAnswers unshownAnswers
      _ -> do
        outputStrLn "Enter a formula with free variables to search for answers."
        mainLoop fileClauses last
  Debug ->
    case last of
      NoHistory -> do
        outputStrLn "No history."
        mainLoop fileClauses last
      EntailmentCheck (SearchState used unused) _ -> do
        outputStrLn "Used clauses:"
        outputStrLn $ prettyPrintAll used
        outputStrLn "Unused clauses:"
        outputStrLn $ prettyPrintAll unused
        mainLoop fileClauses last
      AnswerExtraction _ _ (SearchState used unused) _ _ -> do
        outputStrLn "Used clauses:"
        outputStrLn $ prettyPrintAll used
        outputStrLn "Unused clauses:"
        outputStrLn $ prettyPrintAll unused
        mainLoop fileClauses last

handleFormula :: Set Clause -> Formula -> InputT IO ()
handleFormula fileClauses fo =
  if Set.null free
    then handleClosedFormula fileClauses fo
    else handleOpenFormula fileClauses fo (Set.toAscList free)
  where
    free = Formula.freeVars fo

handleClosedFormula :: Set Clause -> Formula -> InputT IO ()
handleClosedFormula fileClauses fo =
  let negated = FNot fo
      premises = fileClauses `Set.union` Clause.fromFormula negated
   in case unsatisfiable premises of
        (searchState, Nothing) -> do
          outputStrLn "No proof of entailment found."
          mainLoop fileClauses $ EntailmentCheck searchState Nothing
        (searchState, Just (goal, proof)) -> do
          outputStrLn "Entailed."
          mainLoop fileClauses $ EntailmentCheck searchState (Just (goal, proof))

handleOpenFormula :: Set Clause -> Formula -> [Var] -> InputT IO ()
handleOpenFormula fileClauses fo free =
  let answer = FAtom (RltnConst "_goal" (length free)) (TVar <$> free)
      rule = fo `FImp` answer
      premises = fileClauses `Set.union` Clause.fromFormula rule
      searchState = SearchState.initState premises
      terminationCheck =
        ( \clause -> case Set.toList clause of
            [LPos (RltnConst "_goal" _) _] -> True
            _ -> False
        )
      (searchState', results) = resolutionLoop terminationCheck searchState
   in case results of
        [] -> do
          outputStrLn "No answers found."
          mainLoop fileClauses $ AnswerExtraction free terminationCheck searchState' [] []
        ((answer, proof) : rest) -> do
          outputStr $ showAnswer free answer
          mainLoop fileClauses $ AnswerExtraction free terminationCheck searchState' [(answer, proof)] rest

findAnswers :: Set Clause -> [Var] -> (Clause -> Bool) -> SearchState -> [(Clause, IndexedProof)] -> [(Clause, IndexedProof)] -> InputT IO ()
findAnswers fileClauses vars terminationCheck searchState shownAnswers unshownAnswers =
  case unshownAnswers of
    ((nextAnswer, nextProof) : restUnshown) -> do
      outputStr $ showAnswer vars nextAnswer
      mainLoop fileClauses $ AnswerExtraction vars terminationCheck searchState ((nextAnswer, nextProof) : shownAnswers) restUnshown
    [] ->
      let (searchState', newResults) = resolutionLoop terminationCheck searchState
       in case newResults of
            ((nextAnswer, nextProof) : restNew) -> do
              outputStr $ showAnswer vars nextAnswer
              mainLoop fileClauses $ AnswerExtraction vars terminationCheck searchState' ((nextAnswer, nextProof) : shownAnswers) restNew
            [] -> do
              outputStrLn "No additional answers found."
              mainLoop fileClauses $ AnswerExtraction vars terminationCheck searchState' shownAnswers []

showAnswer :: [Var] -> Clause -> String
showAnswer vars clause = case Set.toList clause of
  [LPos _ args] -> unlines $ showVarEqTrm <$> zip vars args
  _ -> undefined
  where
    showVarEqTrm (v, t) = show v ++ " = " ++ show t
