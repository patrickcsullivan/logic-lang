module Repl
  ( repl,
  )
where

import Control.Monad.IO.Class (liftIO)
import Parser (formulaReplP)
import Scanner (spaceConsumer)
import Syntax.Formula (Formula)
import System.Console.Haskeline
  ( defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )
import Text.Megaparsec (errorBundlePretty, parse)

repl :: [Formula] -> IO ()
repl fos = runInputT defaultSettings loop
  where
    loop = do
      maybeInput <- getInputLine "?> "
      case maybeInput of
        Nothing -> loop
        Just "quit" -> outputStrLn "Quiting."
        Just input -> liftIO (process input) >> loop

process :: String -> IO ()
process input = do
  case parse formulaReplP "repl" input of
    Left err -> putStr $ errorBundlePretty err
    Right fo -> print fo
