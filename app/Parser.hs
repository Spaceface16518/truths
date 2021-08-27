{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_HADDOCK hide #-}

module Parser
  ( parseExpr,
  )
where

import Lib (Expr (..))
import Text.ParserCombinators.Parsec
  ( char,
    choice,
    eof,
    letter,
    parse,
    spaces,
    string,
    try,
    (<|>),
  )
import Text.ParserCombinators.Parsec.Error (ParseError)
import Text.ParserCombinators.Parsec.Pos (SourceName)
import Text.ParserCombinators.Parsec.Prim (GenParser)

parseExpr :: SourceName -> String -> Either ParseError Expr
parseExpr = parse statement

statement :: GenParser Char st Expr
statement = do
  spaces
  x <- try binary <|> expr
  spaces
  eof
  return x

expr :: GenParser Char st Expr
expr = choice [binaryP, negation, variable]

variable :: GenParser Char st Expr
variable = do
  c <- letter
  return $ Var c

negation :: GenParser Char st Expr
negation = do
  char '~'
  spaces
  x <- expr
  return $ Not x

binaryP :: GenParser Char st Expr
binaryP = do
  char '('
  spaces
  x <- binary
  spaces
  char ')'
  return x

binary :: GenParser Char st Expr
binary = do
  x1 <- expr
  spaces
  s <- choice $ map string ["&", "|", "->", "<->"]
  spaces
  x2 <- expr
  return $ connective s x1 x2
  where
    connective c = case c of
      "&" -> And
      "|" -> Or
      "->" -> Cond
      "<->" -> Bicond
      _ -> error "Impossible case"
