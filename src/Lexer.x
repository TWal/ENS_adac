{
module Lexer (runAlex, alexMonadScan, scanner) where
}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+                         ;
    "--".*                          ;
    $digit+                         { tok_read TokenInt }
    \' . \'                         { tok' $ TokenChar . head . tail }
    -- reserved words
    "access"                        { tok TokenAccess }
    "false"                         { tok TokenFalse }
    "loop"                          { tok TokenLoop }
    "procedure"                     { tok TokenProcedure }
    "true"                          { tok TokenTrue }
    "and"                           { tok TokenAnd }
    "for"                           { tok TokenFor }
    "new"                           { tok TokenNew }
    "record"                        { tok TokenRecord }
    "type"                          { tok TokenType }
    "begin"                         { tok TokenBegin }
    "function"                      { tok TokenFunction }
    "not"                           { tok TokenNot }
    --"rem"                           { tok TokenRem }
    "use"                           { tok TokenUse }
    "else"                          { tok TokenElse }
    "if"                            { tok TokenIf }
    "null"                          { tok TokenNull }
    "return"                        { tok TokenReturn }
    "while"                         { tok TokenWhile }
    "elsif"                         { tok TokenElsif }
    "in"                            { tok TokenIn }
    "or"                            { tok TokenOr }
    "reverse"                       { tok TokenReverse }
    "with"                          { tok TokenWith }
    "end"                           { tok TokenEnd }
    "is"                            { tok TokenIs }
    "out"                           { tok TokenOut }
    "then"                          { tok TokenThen }
    -- operations
    "="                             { tok TokenEqual }
    "/="                            { tok TokenNotEqual }
    "<"                             { tok TokenLower }
    "<="                            { tok TokenLowerEqual }
    ">"                             { tok TokenGreater }
    ">="                            { tok TokenGreaterEqual }
    "+"                             { tok TokenAdd }
    "-"                             { tok TokenSubtract }
    "*"                             { tok TokenMultiply }
    "("                             { tok TokenLParent }
    ")"                             { tok TokenRParent }
    "rem"                           { tok TokenRem }
    -- Some other characters 
    ":="                            { tok TokenAssign }
    ";"                             { tok TokenSemicolon }
    "."                             { tok TokenDot }
    ":"                             { tok TokenColon }
    -- identifier
    $alpha [$alpha $digit \_ ]*     { tok_string TokenIdent }

{

--tok' :: (String -> TokenClass) -> AlexInput -> Int64 -> Alex Token
tok' :: (String -> TokenClass) -> AlexAction Token
tok' f (p,_,_,s) i = return $ Token p (f (take i s))

tok :: TokenClass -> AlexAction Token
tok x = tok' (const x)

tok_read :: (Integer -> TokenClass) -> AlexAction Token
tok_read x = tok' $ \s -> x (read s)

tok_string :: (String -> TokenClass) -> AlexAction Token
tok_string x = tok' $ \s -> x s

alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TokenEOF


scanner :: String -> Either String [Token]
scanner str =
    let loop = do
                t <- alexComplementError alexMonadScan
                either lexerError (\tk@(Token _ cl) ->
                    if (cl == TokenEOF)
                       then return [tk]
                       else do toks <- loop
                               return (tk : toks)
                 ) t
    in  runAlex str loop

-- TODO: change this awful hack
alexComplementError :: Alex a -> Alex (Either String a)
alexComplementError (Alex al) = Alex (\s -> case al s of
                                             Right (s', x) -> Right (s', Right x)
                                             Left  message -> Right (s, Left message)
                                )


showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':': show col


lexerError :: String -> Alex a
lexerError msg =
    do (p, c, _, inp) <- alexGetInput
       let inpLine = filter (/= '\r') (takeWhile (/='\n') inp)
       let inpTrim = if (length inpLine > 30)
                     then trim (take 30 inpLine)
                     else trim inpLine
       let disp = if (null inp)
                     then " at end of file"
                     else if (null inpTrim)
                             then " before end of line"
                             else " on char " ++ show c ++ " before : '" ++ inpTrim ++ "'"
       let disp3 = if (null msg)
                      then "Lexer error"
                      else trim msg
       alexError (disp3 ++ " at " ++ showPosn p ++ disp)
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

data Token = Token AlexPosn TokenClass deriving (Eq, Show)

data TokenClass =
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
    | TokenEOF
    deriving (Eq, Show)

}
