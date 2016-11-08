{
module Parser (parseExp) where
import Lexer
import AST
}

%name parse expr
%tokentype { Token }
%error { happyError }
%monad { Alex }
%lexer { lexWrap } { TokenEOF _ }

%token
    int         { (TokenInt $$) }
    char        { (TokenChar $$) }
    identt      { (TokenIdent $$) }
    access      { TokenAccess $$ }
    false       { TokenFalse $$ }
    loop        { TokenLoop $$ }
    procedure   { TokenProcedure $$ }
    true        { TokenTrue $$ }
    and         { TokenAnd $$ }
    for         { TokenFor $$ }
    new         { TokenNew $$ }
    record      { TokenRecord $$ }
    type        { TokenType $$ }
    begin       { TokenBegin $$ }
    function    { TokenFunction $$ }
    not         { TokenNot $$ }
    use         { TokenUse $$ }
    else        { TokenElse $$ }
    if          { TokenIf $$ }
    null        { TokenNull $$ }
    return      { TokenReturn $$ }
    while       { TokenWhile $$ }
    elsif       { TokenElsif $$ }
    in          { TokenIn $$ }
    or          { TokenOr $$ }
    reverse     { TokenReverse $$ }
    with        { TokenWith $$ }
    end         { TokenEnd $$ }
    is          { TokenIs $$ }
    out         { TokenOut $$ }
    then        { TokenThen $$ }
    '='         { TokenEqual $$ }
    '/='        { TokenNotEqual $$ }
    '<'         { TokenLower $$ }
    '<='        { TokenLowerEqual $$ }
    '>'         { TokenGreater $$ }
    '>='        { TokenGreaterEqual $$ }
    '+'         { TokenAdd $$ }
    '-'         { TokenSubtract $$ }
    '*'         { TokenMultiply $$ }
    '/'         { TokenDivide $$ }
    '('         { TokenLParent $$ }
    ')'         { TokenRParent $$ }
    rem         { TokenRem $$ }
    ':='        { TokenAssign $$ }
    ';'         { TokenSemicolon $$ }
    '.'         { TokenDot $$ }
    ':'         { TokenColon $$ }
    ','         { TokenComma $$ }

%left or ORELSE
%left and ANDTHEN
%left not
%left '=' '/='
%left '>' '>=' '<' '<='
%left '+' '-'
%left '*' '/'  rem
%left NEG
%left '.'

%%

ident :: { Ann Ident AlexPosn }
ident : identt { (Ident (snd $1), fst $1) }

binop :: { Ann Binop AlexPosn }
binop : '='                     { (Equal, $1) }
      | '/='                    { (NotEqual, $1) }
      | '<'                     { (Lower, $1) }
      | '<='                    { (LowerEqual, $1) }
      | '>'                     { (Greater, $1) }
      | '>='                    { (GreaterEqual, $1) }
      | '+'                     { (Add, $1) }
      | '-'                     { (Subtract, $1) }
      | '*'                     { (Multiply, $1) }
      | '/'                     { (Divide, $1) }
      | rem                     { (Rem, $1) }
      | and                     { (And, $1) }
      | and then %prec ANDTHEN  { (AndThen, $1) }
      | or                      { (Or, $1) }
      | or else %prec ORELSE    { (OrElse, $1) }

expr :: { Ann Expr AlexPosn }
expr : int                     { (EInt (snd $1), fst $1) }
     | char                    { (EChar (snd $1), fst $1) }
     | acces                   { (EAcces $1, snd $1) }
     | true                    { (EBool True, $1) }
     | false                   { (EBool False, $1) }
     | null                    { (ENull, $1) }
     | '(' expr ')'            { $2 }
     | acces                   { (EAcces $1, snd $1) }
     | expr binop expr         { (EBinop $2 $1 $3, snd $1) }
     | not expr                { (EUnop (Not, $1) $2, $1) }
     | '-' expr %prec NEG      { (EUnop (Negate, $1) $2, $1) }
     | new ident               { (ENew $2, $1) }
     | ident '(' exprlist ')'  { (ECall $1 $3, snd $1) }
     -- charval

exprlist :: { NonEmptyList (Ann Expr AlexPosn) }
exprlist : expr               { Last $1 }
         | expr ',' exprlist  { Cons $1 $3 }

acces :: { Ann Acces AlexPosn }
acces : ident           { (AccesIdent $1, snd $1) }
      | expr '.' ident  { (AccesDot $1 $3, snd $1) }


{

lexWrap :: (Token -> Alex a) -> Alex a
lexWrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError t = alexError' ("parse error: " ++ "blabla")

parseExp :: String -> FilePath -> Either String (Ann Expr AlexPosn)
parseExp s fp = runAlex' s fp parse

}
