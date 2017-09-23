
module Middle.ITree where
import Middle.Utility

data Unop = Minc
          | Mdec
          | Mneg
          | Mnot

data Binop = Madd
           | Msub
           | Mimul
           | Mxor
           | Mor
           | Mand
           | Midiv
           | Mdiv
           | Msal
           | Msar
           | Mshl
           | Mcmp

data Expr = Ebinop  Binop Expr Expr
          | Eunop   Unop Expr
          | Eval    [Byte]
          | Enull
          | Emalloc Size
          | Eptr    Expr
          | Evar    String
          | Ecall   String [Expr]

data Instr = IassignPtr Ptr Expr
           | Iassign    String Expr
           | Ieval      Expr
           | Ireturn    (Maybe Expr)
           | Iif        Expr [Instr] [Instr]
           | Ilabel     String
           | Igoto      String

data DeclFunc = DeclFunc
              { dfuncName  :: String
              , dfuncArgs  :: [(String,Size)]
              , dfuncDecls :: [(String,Size)]
              , dfuncBody  :: [Instr]
              , dfuncRet   :: Size
              }

data File = File
          { fileName  :: String
          , fileFuncs :: [DeclFunc]
          , fileDecls :: [(String,Size)]
          , fileBody  :: [Instr]
          }

