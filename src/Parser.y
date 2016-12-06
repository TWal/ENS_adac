{
module Parser (parseExp, parseFichier) where
import Lexer
import AST
}

%name parseExpn expr
%name parseFichiern fichier
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
    typet        { TokenType $$ }
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
    '..'       { TokenDoubledot $$ }
    charval     { TokenCharval $$ }

%nonassoc '..'
%left or ORELSE
%left and ANDTHEN
%nonassoc not
%nonassoc '=' '/='
%nonassoc '>' '>=' '<' '<='
%left '+' '-'
%left '*' '/'  rem
%nonassoc NEG
%left '.'

%%

ident :: { Ann Ident Position }
ident : identt { (Ident (snd $1), fst $1) }

expr :: { Ann Expr Position }
expr : int                               {% if (snd $1) > 2^31 then alexError' "integer too large" else return (EInt (snd $1), fst $1) }
     | char                              { (EChar (snd $1), fst $1) }
     | acces                             { (EAcces $1, snd $1) }
     | true                              { (EBool True, $1) }
     | false                             { (EBool False, $1) }
     | null                              { (ENull, $1) }
     | '(' expr ')'                      { $2 }
     | acces                             { (EAcces $1, snd $1) }
     | expr '=' expr                     { (EBinop (Equal, $2) $1 $3, snd $1) }
     | expr '/=' expr                    { (EBinop (NotEqual, $2) $1 $3, snd $1) }
     | expr '<' expr                     { (EBinop (Lower, $2) $1 $3, snd $1) }
     | expr '<=' expr                    { (EBinop (LowerEqual, $2) $1 $3, snd $1) }
     | expr '>' expr                     { (EBinop (Greater, $2) $1 $3, snd $1) }
     | expr '>=' expr                    { (EBinop (GreaterEqual, $2) $1 $3, snd $1) }
     | expr '+' expr                     { (EBinop (Add, $2) $1 $3, snd $1) }
     | expr '-' expr                     { (EBinop (Subtract, $2) $1 $3, snd $1) }
     | expr '*' expr                     { (EBinop (Multiply, $2) $1 $3, snd $1) }
     | expr '/' expr                     { (EBinop (Divide, $2) $1 $3, snd $1) }
     | expr rem expr                     { (EBinop (Rem, $2) $1 $3, snd $1) }
     | expr and expr                     { (EBinop (And, $2) $1 $3, snd $1) }
     | expr and then expr %prec ANDTHEN  { (EBinop (AndThen, $2) $1 $4, snd $1) }
     | expr or expr                      { (EBinop (Or, $2) $1 $3, snd $1) }
     | expr or else expr %prec ORELSE    { (EBinop (OrElse, $2) $1 $4, snd $1) }
     | not expr                          { (EUnop (Not, $1) $2, $1) }
     | '-' expr %prec NEG                { (EUnop (Negate, $1) $2, $1) }
     | new ident                         { (ENew $2, $1) }
     | ident '(' exprlist ')'            { (ECall $1 $3, snd $1) }
     | charval '(' expr ')'              { (ECharval $3, $1) }

exprlist :: { NonEmptyList (Ann Expr Position) }
exprlist : expr               { Last $1 }
         | expr ',' exprlist  { Cons $1 $3 }

instr :: { Ann Instr Position }
instr : acces ':=' expr ';'                     { (IAssign $1 $3, snd $1) }
      | ident ';'                               { (IIdent $1, snd $1) }
      | ident '(' exprlist ')' ';'              { (ICall $1 $3, snd $1) }
      | return ';'                              { (IReturn Nothing, $1) }
      | return expr ';'                         { (IReturn (Just $2), $1) }
      | begin instrlist end ';'                 { (IBegin $2, $1) }
      | if expr then instrlist elsiflist maybeelse end if ';'
                                                { (IIf $2 $4 $5 $6, $1) }
      | for ident in maybereverse expr '..' expr loop instrlist end loop ';'
                                                { (IFor $2 $4 $5 $7 $9, $1) }
      | while expr loop instrlist end loop ';'  { (IWhile $2 $4, $1) }

instrlist :: { NonEmptyList (Ann Instr Position) }
instrlist : instr            { Last $1 }
          | instr instrlist  { Cons $1 $2 }

elsiflist :: { [((Ann Expr Position), (NonEmptyList (Ann Instr AlexPosn)))] }
elsiflist : {- empty -}                          { [] }
          | elsif expr then instrlist elsiflist  { ($2, $4):$5 }

maybeelse :: { (Maybe (NonEmptyList (Ann Instr Position))) }
maybeelse : {- empty -}     { Nothing }
          | else instrlist  { Just $2 }

maybereverse :: { Bool }
maybereverse : {- empty -}  { False }
             | reverse      { True }

acces :: { Ann Acces Position }
acces : ident           { (AccesIdent $1, snd $1) }
      | expr '.' ident  { (AccesDot $1 $3, snd $1) }


decl :: { Ann Decl Position }
decl : typet ident ';'                                  { (DType $2, $1) }
     | typet ident is new ident ';'                     { (DAlias $2 $5, $1) }
     | typet ident is access ident ';'                  { (DAccess $2 $5, $1) }
     | typet ident is record champslist end record ';'  { (DRecord $2 $5, $1) }
     | identlist ':' type ';'                           { (DAssign $1 $3 Nothing, fstElemAnn $1) }
     | identlist ':' type ':=' expr ';'                 { (DAssign $1 $3 (Just $5), fstElemAnn $1) }
     | procedure ident maybeparams is decllist begin instrlist end maybeident ';'
                {% if maybe True (\x -> fst x == fst $2) $9 then return (DProcedure $2 $3 $5 $7 $9, $1) else alexError' "Procedure name not equal" }
     | function ident maybeparams return type is decllist begin instrlist end maybeident ';'
                {% if maybe True (\x -> fst x == fst $2) $11 then return (DFunction $2 $3 $5 $7 $9 $11, $1) else alexError' "Function name not equal" }

maybeparams :: { Maybe (Ann Params Position) }
maybeparams : {- empty -}  { Nothing }
            | params       { Just $1 }

decllist :: { [Ann Decl Position] }
decllist : {- empty -}    { [] }
         | decl decllist  { $1:$2 }

maybeident :: { Maybe (Ann Ident Position) }
maybeident : {- empty -}  { Nothing }
           | ident        { Just $1 }

champslist :: { NonEmptyList (Ann Champs Position) }
champslist : champs             { Last $1 }
           | champs champslist  { Cons $1 $2 }

identlist :: { NonEmptyList (Ann Ident Position) }
identlist : ident                { Last $1 }
          | ident ',' identlist  { Cons $1 $3 }

champs :: { Ann Champs Position }
champs : identlist ':' type ';'  { (Champs $1 $3, fstElemAnn $1) }

type :: { Ann Type Position }
type : ident         { (NoAccess $1, snd $1) }
     | access ident  { (Access $2, $1) }

params :: { Ann Params Position }
params : '(' paramlist ')'  { (Params $2, $1) }

param :: { Ann Param Position }
param : identlist ':' maybemode type  { (Param $1 $3 $4, fstElemAnn $1) }

paramlist :: { NonEmptyList (Ann Param Position) }
paramlist : param                { Last $1 }
          | param ';' paramlist  { Cons $1 $3 }

maybemode :: { Maybe (Ann Mode Position) }
maybeMode : {- empty -}  { Nothing }
          | in           { Just (In, $1) }
          | in out       { Just (InOut, $1) }

fichier :: { Ann Fichier Position }
fichier : with adatextio ';' use adatextio ';' procedure ident is decllist begin instrlist end maybeident ';'
                {%if maybe True (\x -> fst x == fst $8) $14 then return (Fichier $8 $10 $12 $14, $1) else alexError' "procedure name not equal" }

adatextio :: { () }
adatextio : ident '.' ident {% if (identToStr $1) /= "ada" && (identToStr $3) /= "text_io" then alexError' "Blehbleh" else return () }

{

lexWrap :: (Token -> Alex a) -> Alex a
lexWrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError t = alexError' ("parse error: at token " ++ show t)

parseExp :: String -> FilePath -> Either String (Ann Expr Position)
parseExp s fp = runAlex' s fp parseExpn

parseFichier :: String -> FilePath -> Either String (Ann Fichier Position)
parseFichier s fp = runAlex' s fp parseFichiern

fstElemAnn :: NonEmptyList (Ann a b) -> b
fstElemAnn (Last (_, x)) = x
fstElemAnn (Cons (_, x) _) = x

identToStr :: Ann Ident b -> String
identToStr (Ident s, _) = s

}
