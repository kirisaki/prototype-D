module Lib
    ( someFunc
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Functor

data Expr
  = Var String
  | Lambda String Expr
  | Apply Expr Expr
  deriving(Eq, Show)

data Decl
  = Decl String Expr deriving(Eq, Show)

type Parser = Parsec Void String


var :: Parser Expr
var = Var <$> some (alphaNumChar <|> symbolChar)

lambda :: Parser Expr
lambda = Lambda <$> (char '\\' >> space *> some letterChar) <*> (space >> string "->" >> space *> expr)

apply :: Parser (Expr -> Expr -> Expr)
apply = space1 $> Apply

term :: Parser Expr
term = try var <|> try lambda <|> parens expr

expr :: Parser Expr
expr = do
  t <- term
  loop t
  where
    loop e = loop' e <|> pure e
    loop' lhs = do
      op <- apply
      rhs <- term
      loop $ op lhs rhs

parens :: Parser Expr -> Parser Expr
parens = between (char '(' <* space) (space *> char ')')

someFunc :: IO ()
someFunc = parseTest expr "(\\x -> add x x)"
