{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (Apply, Basic, Expr, pExpr) where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

data Expr
  = Apply Apply -- <apply>
  | Lambda Name Expr -- <name> '->' <expr>
  deriving (Eq, Ord, Show)

data Apply
  = Basic Basic -- <basic>
  | Apply' Apply Basic -- <apply> <basic>
  deriving (Eq, Ord, Show)

data Basic
  = Integer Integer -- <integer>
  | Name Name -- <name>
  | Expr' Expr -- '(' <expr> ')'
  | Pairs [Pair] -- '{' [<pairs>] '}'
  deriving (Eq, Ord, Show)

data Pair = Pair Name Expr -- <name> = <expr>
  deriving (Eq, Ord, Show)

type Name = String

pExpr :: Parser Expr
pExpr =
  choice
    [ try $ do
        name <- pName
        _ <- space *> symbol "->"
        Lambda name <$> pExpr,
      Apply <$> pApply
    ]

pApply :: Parser Apply
pApply = do
  basic <- Basic <$> pBasic
  rest <- many (space *> pBasic)
  return $ foldl Apply' basic rest

pBasic :: Parser Basic
pBasic =
  choice
    [ Integer <$> pInteger,
      Name <$> pName,
      Expr'
        <$> parens
          pExpr,
      Pairs <$> braces pPairs
    ]

pPairs :: Parser [Pair]
pPairs = sepBy1 pPair (symbol ",")

pPair :: Parser Pair
pPair = do
  name <- pName
  _ <- space *> symbol "=" *> space
  Pair name <$> pExpr

pInteger :: Parser Integer
pInteger = lexeme L.decimal

pName :: Parser Name
pName = (:) <$> letterChar <*> many alphaNumChar

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")
