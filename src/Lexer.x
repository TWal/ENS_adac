{
module Lexer (
    runAlex',
    alexMonadScan',
    alexError',
    scanner,
    Alex,
    Position(..),
    Token(..)
) where

import Control.Monad (liftM)
import Data.Char (toLower)
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+                         ;
    "--".*                          ;
    $digit+                                           { tok_read TokenInt }
    \' . \'                                           { tok_char TokenChar }
    -- operations
    "="                                               { tok TokenEqual }
    "/="                                              { tok TokenNotEqual }
    "<"                                               { tok TokenLower }
    "<="                                              { tok TokenLowerEqual }
    ">"                                               { tok TokenGreater }
    ">="                                              { tok TokenGreaterEqual }
    "+"                                               { tok TokenAdd }
    "-"                                               { tok TokenSubtract }
    "*"                                               { tok TokenMultiply }
    "/"                                               { tok TokenDivide }
    "("                                               { tok TokenLParent }
    ")"                                               { tok TokenRParent }
    "rem"                                             { tok TokenRem }
    -- Some other characters
    ":="                                              { tok TokenAssign }
    ";"                                               { tok TokenSemicolon }
    "."                                               { tok TokenDot }
    ":"                                               { tok TokenColon }
    ","                                               { tok TokenComma }
    ".."                                              { tok TokenDoubledot }
    [cC][hH][aA][rR][aA][cC][tT][eE][rR]'[vV][aA][lL] { tok TokenCharval }
    -- identifier and reserved
    $alpha [$alpha $digit \_ ]*                       { tok_string TokenIdent }

{

tok' :: (Position -> String -> Token) -> AlexAction Token
tok' f (p,_,_,s) i = getFilePath >>= (\fp -> return $ (f (Position p fp) (take i s)))

tok :: (Position -> Token) -> AlexAction Token
tok x = tok' $ \p _ -> x p

reserved = [ ("access", TokenAccess),  ("false",TokenFalse),
             ("loop",TokenLoop),       ("procedure",TokenProcedure),
             ("true",TokenTrue),       ("and",TokenAnd),
             ("for",TokenFor),         ("new",TokenNew),
             ("record",TokenRecord),   ("type",TokenType),
             ("begin",TokenBegin),     ("function",TokenFunction),
             ("not",TokenNot),         ("use",TokenUse),
             ("else",TokenElse),       ("if",TokenIf),
             ("null",TokenNull),       ("return",TokenReturn),
             ("while",TokenWhile),     ("elsif",TokenElsif),
             ("in",TokenIn),           ("or",TokenOr),
             ("reverse",TokenReverse), ("with",TokenWith),
             ("end",TokenEnd),         ("is",TokenIs),
             ("out",TokenOut),         ("then",TokenThen) ]

tok_read :: ((Position, Integer) -> Token) -> AlexAction Token
tok_read x = tok' $ \p s -> x (p, (read s))

tok_string :: ((Position, String) -> Token) -> AlexAction Token
tok_string x = tok' $ \p s' ->
                          let s = map toLower s' in
                          let d = lookup s reserved in
                          maybe (x (p, s)) ($ p) d

tok_char :: ((Position, Char) -> Token) -> AlexAction Token
tok_char x = tok' $ \p s -> x (p, (head . tail $ s))

alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  fp <- getFilePath
  return $ TokenEOF (Position p fp)

scanner :: String -> FilePath -> Either String [Token]
scanner str fp =
    let loop = do
                t <- alexMonadScan'
                case t of
                  TokenEOF _ -> return [t]
                  _ -> do
                        toks <- loop
                        return (t : toks)
    in runAlex' str fp loop

data AlexUserState = AlexUserState FilePath

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

-- almost identical to the one generated by alex
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (_,_,_,s) -> alexError' $ "lexical error at character '" ++ take 1 s ++ "'" --line changed
    AlexSkip  inp' _len -> do
        alexSetInput inp'
        alexMonadScan' --line changed
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

setFilePath :: FilePath -> Alex ()
setFilePath fp = alexSetUserState $ AlexUserState fp

getFilePath :: Alex FilePath
getFilePath = liftM (\(AlexUserState fp) -> fp) alexGetUserState

runAlex' :: String -> FilePath -> Alex a -> Either String a
runAlex' s fp a = runAlex s $
    do setFilePath fp
       a

alexError' :: String -> Alex a
alexError' msg =
    do
      fp <- getFilePath
      (pos, _, _, _) <- alexGetInput
      alexError ((show (Position pos fp)) ++ ":\n" ++ msg)

data Position = Position AlexPosn FilePath deriving (Eq)

instance Show Position where
    show (Position (AlexPn _ l c) fp) = "File \"" ++ fp ++ "\", line " ++ show l ++ ", characters " ++ show c ++ "-" ++ show (c+1)

data Token =
      TokenInt (Position, Integer)
    | TokenIdent (Position, String)
    | TokenChar (Position, Char)
    | TokenAccess Position
    | TokenFalse Position
    | TokenLoop Position
    | TokenProcedure Position
    | TokenTrue Position
    | TokenAnd Position
    | TokenFor Position
    | TokenNew Position
    | TokenRecord Position
    | TokenType Position
    | TokenBegin Position
    | TokenFunction Position
    | TokenNot Position
    | TokenUse Position
    | TokenElse Position
    | TokenIf Position
    | TokenNull Position
    | TokenReturn Position
    | TokenWhile Position
    | TokenElsif Position
    | TokenIn Position
    | TokenOr Position
    | TokenReverse Position
    | TokenWith Position
    | TokenEnd Position
    | TokenIs Position
    | TokenOut Position
    | TokenThen Position
    | TokenEqual Position
    | TokenNotEqual Position
    | TokenLower Position
    | TokenLowerEqual Position
    | TokenGreater Position
    | TokenGreaterEqual Position
    | TokenAdd Position
    | TokenSubtract Position
    | TokenMultiply Position
    | TokenDivide Position
    | TokenLParent Position
    | TokenRParent Position
    | TokenRem Position
    | TokenAssign Position
    | TokenSemicolon Position
    | TokenDot Position
    | TokenColon Position
    | TokenComma Position
    | TokenDoubledot Position
    | TokenCharval Position
    | TokenEOF Position
    deriving (Eq)


instance Show Token where
    show (TokenInt (_, i)) = show i
    show (TokenIdent (_, s)) = s
    show (TokenChar (_, c)) = '\'' : c : '\'' : []
    show (TokenAccess _) = "access"
    show (TokenFalse _) = "false"
    show (TokenLoop _) = "loop"
    show (TokenProcedure _) = "procedure"
    show (TokenTrue _) = "true"
    show (TokenAnd _) = "and"
    show (TokenFor _) = "for"
    show (TokenNew _) = "new"
    show (TokenRecord _) = "record"
    show (TokenType _) = "type"
    show (TokenBegin _) = "begin"
    show (TokenFunction _) = "function"
    show (TokenNot _) = "not"
    show (TokenUse _) = "use"
    show (TokenElse _) = "else"
    show (TokenIf _) = "if"
    show (TokenNull _) = "null"
    show (TokenReturn _) = "return"
    show (TokenWhile _) = "while"
    show (TokenElsif _) = "elsif"
    show (TokenIn _) = "in"
    show (TokenOr _) = "or"
    show (TokenReverse _) = "reverse"
    show (TokenWith _) = "with"
    show (TokenEnd _) = "end"
    show (TokenIs _) = "is"
    show (TokenOut _) = "out"
    show (TokenThen _) = "then"
    show (TokenEqual _) = "="
    show (TokenNotEqual _) = "/="
    show (TokenLower _) = "<"
    show (TokenLowerEqual _) = "<="
    show (TokenGreater _) = ">"
    show (TokenGreaterEqual _) = ">="
    show (TokenAdd _) = "+"
    show (TokenSubtract _) = "-"
    show (TokenMultiply _) = "*"
    show (TokenDivide _) = "/"
    show (TokenLParent _) = "("
    show (TokenRParent _) = ")"
    show (TokenRem _) = "rem"
    show (TokenAssign _) = ":="
    show (TokenSemicolon _) = ";"
    show (TokenDot _) = "."
    show (TokenColon _) = ":"
    show (TokenComma _) = ","
    show (TokenDoubledot _) = ".."
    show (TokenCharval _) = "character'val"
    show (TokenEOF _) = "<EOF>"
}
