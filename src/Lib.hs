module Lib
    ( someFunc
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Functor

data Expr
  = Var String
  | Bool Bool
  | Lambda String Expr
  | Apply Expr Expr
  | If Expr Expr Expr
  deriving(Eq, Show)

data Decl
  = Decl String Expr deriving(Eq, Show)

type Parser = Parsec Void String


true :: Parser Expr
true = string "True" $> Bool True

false :: Parser Expr
false = string "False" $> Bool True

if_ :: Parser Expr
if_ = If <$> (string "if" *> term) <*> (string "then" *> term) <*> (string "else" *> term)

var :: Parser Expr
var = Var <$> some (alphaNumChar <|> symbolChar)

lambda :: Parser Expr
lambda = Lambda <$> (char '\\' >> space *> some letterChar) <*> (space >> string "->" >> space *> expr)

apply :: Parser (Expr -> Expr -> Expr)
apply = space $> Apply

term :: Parser Expr
term = between space space $ if_ <|> true <|> false <|> try var <|> try lambda <|> parens expr

expr :: Parser Expr
expr = do
  space
  t <- term
  loop t <* space
  where
    loop e = loop' e <|> pure e
    loop' lhs = do
      op <- apply
      rhs <- term
      loop $ op lhs rhs

parens :: Parser Expr -> Parser Expr
parens = between (char '(' <* space) (space *> char ')')

someFunc :: IO ()
someFunc = do
  parseTest expr "(\\x -> add x x)"
  parseTest expr "(\\x -> add x x)12"
  parseTest expr "x y z"
  parseTest expr "if True then x else y"
