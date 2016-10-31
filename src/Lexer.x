{
module Lexer (alexScanTokens) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+                         ;
    "--".*                          ;
    $digit+                         { \s -> TokenInt (read s) }
    \' . \'                         { \s -> TokenChar . head . tail $ s }
    -- reserved words
    "access"                        { const TokenAccess }
    "false"                         { const TokenFalse }
    "loop"                          { const TokenLoop }
    "procedure"                     { const TokenProcedure }
    "true"                          { const TokenTrue }
    "and"                           { const TokenAnd }
    "for"                           { const TokenFor }
    "new"                           { const TokenNew }
    "record"                        { const TokenRecord }
    "type"                          { const TokenType }
    "begin"                         { const TokenBegin }
    "function"                      { const TokenFunction }
    "not"                           { const TokenNot }
    --"rem"                           { const TokenRem }
    "use"                           { const TokenUse }
    "else"                          { const TokenElse }
    "if"                            { const TokenIf }
    "null"                          { const TokenNull }
    "return"                        { const TokenReturn }
    "while"                         { const TokenWhile }
    "elsif"                         { const TokenElsif }
    "in"                            { const TokenIn }
    "or"                            { const TokenOr }
    "reverse"                       { const TokenReverse }
    "with"                          { const TokenWith }
    "end"                           { const TokenEnd }
    "is"                            { const TokenIs }
    "out"                           { const TokenOut }
    "then"                          { const TokenThen }
    -- operations
    "="                             { const TokenEqual }
    "/="                            { const TokenNotEqual }
    "<"                             { const TokenLower }
    "<="                            { const TokenLowerEqual }
    ">"                             { const TokenGreater }
    ">="                            { const TokenGreaterEqual }
    "+"                             { const TokenAdd }
    "-"                             { const TokenSubtract }
    "*"                             { const TokenMultiply }
    "("                             { const TokenLParent }
    ")"                             { const TokenRParent }
    "rem"                           { const TokenRem }
    -- Some other characters 
    ":="                            { const TokenAssign }
    ";"                             { const TokenSemicolon }
    "."                             { const TokenDot }
    ":"                             { const TokenColon }
    -- identifier
    $alpha [$alpha $digit \_ ]*     { \s -> TokenIdent s }

{

data Token =
      TokenInt Integer
    | TokenIdent String
    | TokenChar Char
    | TokenAccess
    | TokenFalse
    | TokenLoop
    | TokenProcedure
    | TokenTrue
    | TokenAnd
    | TokenFor
    | TokenNew
    | TokenRecord
    | TokenType
    | TokenBegin
    | TokenFunction
    | TokenNot
    | TokenUse
    | TokenElse
    | TokenIf
    | TokenNull
    | TokenReturn
    | TokenWhile
    | TokenElsif
    | TokenIn
    | TokenOr
    | TokenReverse
    | TokenWith
    | TokenEnd
    | TokenIs
    | TokenOut
    | TokenThen
    | TokenEqual
    | TokenNotEqual
    | TokenLower
    | TokenLowerEqual
    | TokenGreater
    | TokenGreaterEqual
    | TokenAdd
    | TokenSubtract
    | TokenMultiply
    | TokenLParent
    | TokenRParent
    | TokenRem
    | TokenAssign
    | TokenSemicolon
    | TokenDot
    | TokenColon
    deriving (Eq, Show)

}
