{-# LANGUAGE TupleSections #-}
module Lib
    ( someFunc
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Functor
import qualified Data.Set as SE
import Control.Monad
import qualified Text.Megaparsec.Char.Lexer as Lx
import qualified Data.List as L

data Expr
  = Var String
  | Bool Bool
  | Lambda String Expr
  | Apply Expr Expr
  | If Expr Expr Expr
  | Let [Decl] Expr
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
  , "let"
  , "in"
  ]

if_, then_, else_, let_, in_ :: String
if_ = "if"
then_ = "then"
else_ = "else"
let_ = "let"
in_ = "in"

ifExpr :: Parser Expr
ifExpr = If <$> (string if_ *> expr) <*> (string then_ *> expr) <*> (string else_ *> expr) 

letExpr :: Parser Expr
letExpr =
  let
    let1 = do
      ds <- Lx.nonIndented scn (Lx.indentBlock scn (string let_ >> pure (Lx.IndentSome Nothing pure decl)))
      e:es <- Lx.nonIndented scn (Lx.indentBlock scn (string in_ >> pure (Lx.IndentSome Nothing pure expr)))
      unless (L.null es) $ failure Nothing SE.empty
      pure $ Let ds e
    let2 = Let <$> (string let_ *> space *> ((:[]) <$> decl)) <*> (space *> string in_ *> expr)
  in
    try let1 <|> let2

      
var :: Parser Expr
var = do
  sym <- some (alphaNumChar <|> symbolChar)
  when (sym `elem` reserved) (failure Nothing SE.empty)
  pure $ Var sym

lambda :: Parser Expr
lambda = Lambda <$> (char '\\' >> space *> some letterChar) <*> (space *> string "->" *> space *> expr)


term :: Parser Expr
term =  true <|> false <|> var <|> lambda <|> parens expr

apply :: Parser Expr
apply = do
  t <- term
  loop t
  where
    loop e = try (loop' e) <|> pure e
    loop' lhs = do
      op <- lexeme $ pure Apply
      rhs <- term
      loop $ op lhs rhs

expr :: Parser Expr
expr = between sc sc $ ifExpr <|> letExpr <|> apply <|> term

parens :: Parser Expr -> Parser Expr
parens = between (char '(' <* space) (space *> char ')')

decl :: Parser Decl
decl = Decl <$> some (alphaNumChar <|> symbolChar) <*> (space *> char '=' *> expr)

lineComment :: Parser ()
lineComment = Lx.skipLineComment "--"

scn :: Parser ()
scn = Lx.space space1 lineComment empty

sc :: Parser ()
sc = Lx.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = Lx.lexeme sc

someFunc :: IO ()
someFunc = do
  parseTest expr "( \\x -> add x x)"
  parseTest expr "( \\x -> add x x)12"
  parseTest expr "x y z "
  parseTest expr "if f x then y else z"

  parseTest expr "let x = 1 in x"
  parseTest expr $
    "let\n" <>
    "  x = 1\n" <>
    "  y = 1\n" <>
    "in\n" <>
    "  x\n"

