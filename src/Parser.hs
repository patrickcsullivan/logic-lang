module Parser (formulaP, parse') where

import Control.Applicative (liftA2, liftA3)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR, Prefix), makeExprParser)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import qualified Scanner
import Syntax.Constant (FnConst (..), ObjConst (..), RltnConst (..))
import Syntax.Formula (Formula (..))
import Syntax.Term (Term (..))
import Syntax.Variable (Var (..))
import Text.Megaparsec (Parsec, optional, parse, sepBy, some, try, (<?>), (<|>))

type Parser = Parsec Void String

-- | Temporary function for running the formula parser and unwrapping the
-- successful result.
parse' :: String -> Formula
parse' s = case parse formulaP "" s of
  Left _ -> undefined
  Right f -> f

formulaP :: Parser Formula
formulaP = makeExprParser formulaCaseP formulaOperators

formulaOperators :: [[Operator Parser Formula]]
formulaOperators =
  [ [Prefix $ foldr1 (.) <$> some (FNot <$ Scanner.symbol "~")],
    [InfixR (FAnd <$ Scanner.reservedWord "and")],
    [InfixR (FOr <$ Scanner.reservedWord "or")],
    [InfixR (FImp <$ Scanner.reservedWord "==>")],
    [InfixR (FIff <$ Scanner.reservedWord "<=>")]
  ]

formulaCaseP :: Parser Formula
formulaCaseP =
  Scanner.parens formulaP
    <|> try (FFalse <$ Scanner.reservedWord "False")
    <|> try (FTrue <$ Scanner.reservedWord "True")
    <|> forAllP
    <|> existsP
    <|> try prefixRltnP
    <|> infixRltnP

trueP :: Parser Formula
trueP = FTrue <$ Scanner.reservedWord "True"

falseP :: Parser Formula
falseP = FFalse <$ Scanner.reservedWord "False"

prefixRltnP :: Parser Formula
prefixRltnP = do
  name <- Scanner.rltnConstIdentifier
  args <- Scanner.parens (termP `sepBy` Scanner.comma)
  return $ FAtom (RltnConst name (length args)) args

infixRltnP :: Parser Formula
infixRltnP = do
  left <- termP
  name <- Scanner.rltnConstOperator
  right <- termP
  return $ FAtom (RltnConst name 2) [left, right]

forAllP :: Parser Formula
forAllP = do
  Scanner.reservedWord "forall"
  (v : vs) <- reverse <$> some (Scanner.varIdentifier <?> "variable binding")
  Scanner.symbol "."
  scope <- formulaP
  return $ foldl mkForAll (mkForAll scope v) vs
  where
    mkForAll scope varName = FForAll (Var varName) scope

existsP :: Parser Formula
existsP = do
  Scanner.reservedWord "exists"
  (v : vs) <- reverse <$> some (Scanner.varIdentifier <?> "variable binding")
  Scanner.symbol "."
  scope <- formulaP
  return $ foldl mkExists (mkExists scope v) vs
  where
    mkExists scope varName = FExists (Var varName) scope

termP :: Parser Term
termP = makeExprParser termCaseP termOperators

termOperators :: [[Operator Parser Term]]
termOperators =
  [ [InfixR (mkBinOp "^" <$ Scanner.reservedWord "^")],
    [ InfixL (mkBinOp "*" <$ Scanner.reservedWord "*"),
      InfixL (mkBinOp "/" <$ Scanner.reservedWord "/")
    ],
    [ InfixL (mkBinOp "+" <$ Scanner.reservedWord "+"),
      InfixL (mkBinOp "-" <$ Scanner.reservedWord "-")
    ],
    [InfixR (mkBinOp ":" <$ Scanner.reservedWord ":")]
  ]
  where
    mkBinOp name arg1 arg2 = TFn (FnConst name 2) [arg1, arg2]

termCaseP :: Parser Term
termCaseP =
  Scanner.parens termP
    <|> varP
    <|> fnOrObjP

varP :: Parser Term
varP = TVar . Var <$> Scanner.varIdentifier

fnOrObjP :: Parser Term
fnOrObjP = do
  name <- Scanner.fnObjConstIdentifier
  maybeArgs <- optional $ Scanner.parens (termP `sepBy` Scanner.comma)
  let args = fromMaybe [] maybeArgs
  if null args
    then return $ TObj (ObjConst name)
    else return $ TFn (FnConst name (length args)) args
