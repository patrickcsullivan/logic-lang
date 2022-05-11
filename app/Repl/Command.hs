module Repl.Command
  ( Command (..),
    parse,
  )
where

data Command = Quit | LastProof | LastAnswer | NextAnswer | Debug

parse :: String -> Maybe Command
parse s = case s of
  "q" -> Just Quit
  "quit" -> Just Quit
  "p" -> Just LastProof
  "proof" -> Just LastProof
  "a" -> Just LastAnswer
  "answer" -> Just LastAnswer
  "n" -> Just NextAnswer
  "next" -> Just NextAnswer
  "debug" -> Just Debug
  _ -> Nothing
