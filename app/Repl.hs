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
repl fileFormulas = runInputT defaultSettings (mainLoop avoid fileClauses NoHistory)
  where
    -- fileClauses = Set.unions (snd <$> Clause.fromFormula Set.empty <$> fileFormulas)

    (avoid, fileClauses) =
      foldl
        ( \(avoidAcc, clausesAcc) nextFo ->
            let (avoidAcc', nextClauses) = Clause.fromFormula avoidAcc nextFo
             in (avoidAcc', clausesAcc `Set.union` nextClauses)
        )
        (Set.empty, Set.empty)
        fileFormulas -- Set.unions (Clause.fromFormula <$> fileFormulas)

mainLoop :: Set String -> Set Clause -> LastResult -> InputT IO ()
mainLoop avoid fileClauses last = do
  maybeInput <- getInputLine "?> "
  case maybeInput of
    Nothing -> mainLoop avoid fileClauses last
    Just (':' : rest) -> case Command.parse rest of
      Nothing -> do
        outputStrLn "Unrecognized command."
        mainLoop avoid fileClauses last
      Just cmd -> handleCommand avoid fileClauses last cmd
    Just input | all isSpace input -> mainLoop avoid fileClauses last
    Just input -> case parse formulaReplP "repl" input of
      Left err -> do
        outputStr (errorBundlePretty err)
        mainLoop avoid fileClauses last
      Right fo -> handleFormula avoid fileClauses fo

handleCommand :: Set String -> Set Clause -> LastResult -> Command -> InputT IO ()
handleCommand avoid fileClauses last command = case command of
  Quit -> outputStrLn "Quitting."
  LastProof ->
    case last of
      NoHistory -> do
        outputStrLn "There is no proof to show."
        mainLoop avoid fileClauses last
      EntailmentCheck _ Nothing -> do
        outputStrLn "There is no proof to show."
        mainLoop avoid fileClauses last
      EntailmentCheck (SearchState usedClauses _) (Just (goal, proof)) -> do
        let usedClauses' = (\(i, c, p) -> (c, p)) <$> usedClauses
        liftIO (Proof.prettyPrint usedClauses' (goal, proof))
        mainLoop avoid fileClauses last
      AnswerExtraction _ _ (SearchState usedClauses _) ((answer, proof) : _) _ -> do
        let usedClauses' = (\(i, c, p) -> (c, p)) <$> usedClauses
        liftIO (Proof.prettyPrint usedClauses' (answer, proof))
        mainLoop avoid fileClauses last
      AnswerExtraction _ _ _ [] _ -> do
        -- This case will only occur if there were no answers at all.
        outputStrLn "There is no proof to show."
        mainLoop avoid fileClauses last
  LastAnswer ->
    case last of
      NoHistory -> do
        outputStrLn "No answer to show."
        mainLoop avoid fileClauses last
      EntailmentCheck _ (Just _) -> do
        outputStrLn "Entailed."
        mainLoop avoid fileClauses last
      EntailmentCheck _ Nothing -> do
        outputStrLn "No proof of entailment found."
        mainLoop avoid fileClauses last
      AnswerExtraction vars _ _ ((answer, _) : _) _ -> do
        outputStr $ showAnswer vars answer
        mainLoop avoid fileClauses last
      AnswerExtraction _ _ _ [] _ -> do
        -- This case will only occur if there were no answers at all.
        outputStrLn "No answers found."
        mainLoop avoid fileClauses last
  NextAnswer ->
    case last of
      AnswerExtraction vars terminationCheck searchState shownAnswers unshownAnswers -> do
        findAnswers avoid fileClauses vars terminationCheck searchState shownAnswers unshownAnswers
      _ -> do
        outputStrLn "Enter a formula with free variables to search for answers."
        mainLoop avoid fileClauses last
  Debug ->
    case last of
      NoHistory -> do
        outputStrLn "No history."
        mainLoop avoid fileClauses last
      EntailmentCheck (SearchState used unused) _ -> do
        outputStrLn "Used clauses:"
        outputStrLn $ prettyPrintAll used
        outputStrLn "Unused clauses:"
        outputStrLn $ prettyPrintAll unused
        mainLoop avoid fileClauses last
      AnswerExtraction _ _ (SearchState used unused) _ _ -> do
        outputStrLn "Used clauses:"
        outputStrLn $ prettyPrintAll used
        outputStrLn "Unused clauses:"
        outputStrLn $ prettyPrintAll unused
        mainLoop avoid fileClauses last

handleFormula :: Set String -> Set Clause -> Formula -> InputT IO ()
handleFormula avoid fileClauses fo =
  if Set.null free
    then handleClosedFormula avoid fileClauses fo
    else handleOpenFormula avoid fileClauses fo (Set.toAscList free)
  where
    free = Formula.freeVars fo

handleClosedFormula :: Set String -> Set Clause -> Formula -> InputT IO ()
handleClosedFormula avoid fileClauses fo =
  let negated = FNot fo
      (_, newClauses) = Clause.fromFormula avoid negated
      premises = fileClauses `Set.union` newClauses
   in case unsatisfiable premises of
        (searchState, Nothing) -> do
          outputStrLn "No proof of entailment found."
          mainLoop avoid fileClauses $ EntailmentCheck searchState Nothing
        (searchState, Just (goal, proof)) -> do
          outputStrLn "Entailed."
          mainLoop avoid fileClauses $ EntailmentCheck searchState (Just (goal, proof))

handleOpenFormula :: Set String -> Set Clause -> Formula -> [Var] -> InputT IO ()
handleOpenFormula avoid fileClauses fo free =
  let answer = FAtom (RltnConst "_goal" (length free)) (TVar <$> free)
      rule = fo `FImp` answer
      (_, newClauses) = Clause.fromFormula avoid rule
      premises = fileClauses `Set.union` newClauses
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
          mainLoop avoid fileClauses $ AnswerExtraction free terminationCheck searchState' [] []
        ((answer, proof) : rest) -> do
          outputStr $ showAnswer free answer
          mainLoop avoid fileClauses $ AnswerExtraction free terminationCheck searchState' [(answer, proof)] rest

findAnswers :: Set String -> Set Clause -> [Var] -> (Clause -> Bool) -> SearchState -> [(Clause, IndexedProof)] -> [(Clause, IndexedProof)] -> InputT IO ()
findAnswers avoid fileClauses vars terminationCheck searchState shownAnswers unshownAnswers =
  case unshownAnswers of
    ((nextAnswer, nextProof) : restUnshown) -> do
      outputStr $ showAnswer vars nextAnswer
      mainLoop avoid fileClauses $ AnswerExtraction vars terminationCheck searchState ((nextAnswer, nextProof) : shownAnswers) restUnshown
    [] ->
      let (searchState', newResults) = resolutionLoop terminationCheck searchState
       in case newResults of
            ((nextAnswer, nextProof) : restNew) -> do
              outputStr $ showAnswer vars nextAnswer
              mainLoop avoid fileClauses $ AnswerExtraction vars terminationCheck searchState' ((nextAnswer, nextProof) : shownAnswers) restNew
            [] -> do
              outputStrLn "No additional answers found."
              mainLoop avoid fileClauses $ AnswerExtraction vars terminationCheck searchState' shownAnswers []

showAnswer :: [Var] -> Clause -> String
showAnswer vars clause = case Set.toList clause of
  [LPos _ args] -> unlines $ showVarEqTrm <$> zip vars args
  _ -> undefined
  where
    showVarEqTrm (v, t) = show v ++ " = " ++ show t
