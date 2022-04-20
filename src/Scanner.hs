module Scanner where

import Control.Monad (void)
import Data.Char ()
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, many, notFollowedBy, single, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, letterChar, lowerChar, numberChar, space1, string, upperChar)
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void String

-- -----------------------------------------------------------------------------

-- LEXING

-- | Parser that can parse white space and comments.
spaceConsumer :: Parser ()
spaceConsumer = Lexer.space space1 lineComment blockComment
  where
    lineComment = Lexer.skipLineComment "--"
    blockComment = Lexer.skipBlockComment "{-" "-}"

-- | Produces a parser for a lexme.
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

-- | Symbol lexeme parser.
symbol :: String -> Parser String
symbol = Lexer.symbol spaceConsumer

-- | Comma lexeme parser.
comma :: Parser ()
comma = void $ symbol ","

-- | Semicolon lexeme parser.
semi :: Parser ()
semi = void $ symbol ";"

-- | Produces a parser for the contents between parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Produces a parser that attempts to parse the given reserved word lexeme and
-- backtracks if it fails.
reservedWord :: String -> Parser ()
reservedWord w = (lexeme . try) $ do
  string w
  notFollowedBy alphaNumChar

-- | Reserved words that have syntactic meaning.
reservedWords :: [String]
reservedWords = ["True", "False", "and", "or", "==>", "<=>", "forall", "exists"]

-- -----------------------------------------------------------------------------

-- IDENTIFIERS

varIdentifier :: Parser String
varIdentifier = (lexeme . try) $ do
  c <- upperChar <|> single '_'
  cs <- many (alphaNumChar <|> single '_' <|> single '\'')
  let word = c : cs
  if word `elem` reservedWords
    then fail $ "keyword " <> show word <> " cannot be a variable"
    else return word

rltnConstIdentifier :: Parser String
rltnConstIdentifier = (lexeme . try) $ do
  c <- lowerChar
  cs <- many (alphaNumChar <|> single '_' <|> single '\'')
  let word = c : cs
  if word `elem` reservedWords
    then fail $ "keyword " <> show word <> " cannot be a relation constant"
    else return word

fnObjConstIdentifier :: Parser String
fnObjConstIdentifier = (lexeme . try) $ do
  c <- lowerChar <|> numberChar
  cs <- many (alphaNumChar <|> single '_' <|> single '\'')
  let word = c : cs
  if word `elem` reservedWords
    then fail $ "keyword " <> show word <> " cannot be an object constant or function constant"
    else return word
