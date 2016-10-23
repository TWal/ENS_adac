module Parser where

import Control.Monad (void)
import Data.Char (isAlphaNum)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

data Expr = IntCst Integer
           | CharCst Char
           | BoolCst Bool
           | NullCst
        -- | Access Access
           | Binop Binop Expr Expr
           | Unop Unop Expr
           | New Ident
           | FctCall Ident [Expr]
           -- TODO: understand what is the last line
           deriving (Show)

data Binop = Or | OrElse | And | AndThen
           | Equal | NotEqual | Greater | GreaterEq
           | Lower | LowerEq | Add | Subtract
           | Multiply | Divide | Rem deriving (Show)

data Unop = Not | Neg deriving (Show)

data Ident = Ident String deriving (Show)

-- Lexer

-- Space consumer
sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "--") (L.skipBlockComment "{-" "-}") --TODO no block comment!

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.integer

bool :: Parser Bool
bool = lexeme $ (symbol "true" *> return True) <|> (symbol "false" *> return False)

character :: Parser Char
character = symbol "'" *> asciiChar <* symbol "'"

semi :: Parser String
semi = symbol ";"

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws :: [String] -- list of reserved words
rws = ["access", "and", "begin", "else", "elsif", "end",
      "false", "for", "function", "if", "in", "is",
      "loop", "new", "not", "null", "or", "out",
      "procedure", "record", "rem", "return", "reverse", "then",
      "true", "type", "use", "while", "with"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many (satisfy $ (||) <$> isAlphaNum <*> (=='_'))
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x


------- Parser

expr :: Parser Expr
expr = makeExprParser term operators

operators :: [[Operator Parser Expr]]
operators =
  [ [Prefix  (symbol "-" *> pure (Unop Neg)) ]
  , [ InfixL (symbol "*" *> pure (Binop Multiply))
    , InfixL (symbol "/" *> pure (Binop Divide))
    , InfixL (symbol "rem" *> pure (Binop Rem)) ]
  , [ InfixL (symbol "+" *> pure (Binop Add))
    , InfixL (symbol "-" *> pure (Binop Subtract)) ]
  , [ InfixL (symbol ">" *> pure (Binop Greater))
    , InfixL (symbol ">=" *> pure (Binop GreaterEq))
    , InfixL (symbol "<" *> pure (Binop Lower))
    , InfixL (symbol "<=" *> pure (Binop LowerEq)) ]
  , [ InfixL (symbol "=" *> pure (Binop Equal))
    , InfixL (symbol "/=" *> pure (Binop NotEqual)) ]
  , [Prefix  (symbol "not" *> pure (Unop Not)) ]
  , [ InfixL (symbol "and" *> pure (Binop And))
    , InfixL (symbol "and then" *> pure (Binop AndThen)) ]
  , [ InfixL (symbol "or" *> pure (Binop Or))
    , InfixL (symbol "or else" *> pure (Binop OrElse)) ]
  ]

term :: Parser Expr
term = parens expr --TODO: add `try`?
     <|> IntCst <$> integer
     <|> CharCst <$> character
     <|> BoolCst <$> bool
     <|> const NullCst <$> symbol "null"
    -- Access
     <|> New . Ident <$> (symbol "new" *> identifier)
    -- Function call
