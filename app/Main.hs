module Main where

import Parser (formulasFileP)
import Repl (repl)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  file <- openFile path ReadMode
  text <- hGetContents file
  case parse formulasFileP path text of
    Left err -> putStr $ errorBundlePretty err
    Right fos -> repl fos
