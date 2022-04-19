module Parser where

import Ast (Formula (..), RltnConst (RltnConst))
import Control.Applicative (liftA2, liftA3)
import Control.Monad.Combinators.Expr (Operator (InfixR, Prefix), makeExprParser)
import Data.Void (Void)
import qualified Scanner
import Text.Megaparsec (Parsec, some, try, (<|>))

type Parser = Parsec Void String

formulaOperators :: [[Operator Parser Formula]]
formulaOperators =
  [ [Prefix $ foldr1 (.) <$> some (FNot <$ Scanner.symbol "~")],
    [InfixR (FAnd <$ Scanner.reservedWord "and")],
    [InfixR (FOr <$ Scanner.reservedWord "or")],
    [InfixR (FImp <$ Scanner.reservedWord "==>")],
    [InfixR (FIff <$ Scanner.reservedWord "<=>")]
  ]

formulaP :: Parser Formula
formulaP = makeExprParser formulaCaseP formulaOperators

formulaCaseP :: Parser Formula
formulaCaseP =
  Scanner.parens formulaP
    <|> try (FFalse <$ Scanner.reservedWord "False")
    <|> try (FTrue <$ Scanner.reservedWord "True")
    <|> forAllP
    <|> existsP
    <|> atomP

trueP :: Parser Formula
trueP = FTrue <$ Scanner.reservedWord "True"

falseP :: Parser Formula
falseP = FFalse <$ Scanner.reservedWord "False"

atomP :: Parser Formula
atomP = do
  name <- Scanner.rltnConstIdentifier
  return $ FAtom (RltnConst name 0) []

forAllP :: Parser Formula
forAllP = do
  Scanner.reservedWord "forall"
  (v : vs) <- reverse <$> some Scanner.varIdentifier
  Scanner.symbol "."
  scope <- formulaP
  return $ foldl (flip FForAll) (FForAll v scope) vs

existsP :: Parser Formula
existsP = do
  Scanner.reservedWord "exists"
  (v : vs) <- reverse <$> some Scanner.varIdentifier
  Scanner.symbol "."
  scope <- formulaP
  return $ foldl (flip FExists) (FExists v scope) vs