module Lib
    ( someFunc
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Functor
import qualified Data.Set as SE
import Control.Monad

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

reserved :: [String]
reserved =
  [ "if"
  , "then"
  , "else"
  ]

if_ :: Parser Expr
if_ = If <$> (string "if" *> expr) <*> (string "then" *> expr) <*> (string "else" *> expr)

var :: Parser Expr
var = do
  sym <- some (alphaNumChar <|> symbolChar)
  when (sym `elem` reserved) (failure Nothing SE.empty)
  pure $ Var sym

lambda :: Parser Expr
lambda = Lambda <$> (char '\\' >> space *> some letterChar) <*> (space >> string "->" >> space *> expr)


term :: Parser Expr
term = between space space $ true <|> false <|> var <|> lambda <|> parens expr

apply :: Parser Expr
apply = do
  t <- term
  loop t
  where
    loop e = try (loop' e) <|> pure e
    loop' lhs = do
      op <- space $> Apply
      rhs <- term
      loop $ op lhs rhs

expr :: Parser Expr
expr = between space space $ if_ <|> apply <|> term

parens :: Parser Expr -> Parser Expr
parens = between (char '(' <* space) (space *> char ')')

someFunc :: IO ()
someFunc = do
  parseTest expr "(\\x -> add x x)"
  parseTest expr "(\\x -> add x x)12"
  parseTest expr "x y z "
  parseTest expr "if f x then y else z"
