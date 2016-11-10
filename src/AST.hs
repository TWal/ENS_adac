module AST (
    NonEmptyList(..),
    Ann,
    Ident(..),
    Fichier(..),
    Decl(..),
    Champs(..),
    Type(..),
    Params(..),
    Param(..),
    Mode(..),
    Expr(..),
    Instr(..),
    Binop(..),
    Unop(..),
    Acces(..)
) where

data NonEmptyList a = Cons a (NonEmptyList a) | Last a deriving (Eq, Show)

-- a is the "recursion" and b the annoted information
--data WPos a b = WPos a b deriving (Eq, Show)
type Ann a b = (a b, b)

data Ident b = Ident String deriving (Eq, Show)

data Fichier b = Fichier (Ann Ident b) [Ann Decl b] (NonEmptyList (Ann Instr b)) (Maybe (Ann Ident b)) deriving (Eq, Show)

data Decl b =
    DType (Ann Ident b)
  | DAccess (Ann Ident b) (Ann Ident b)
  | DRecord (Ann Ident b) (NonEmptyList (Ann Champs b))
  | DAssign (NonEmptyList (Ann Ident b)) (Ann Type b) (Maybe (Ann Expr b))
  | DProcedure (Ann Ident b) (Maybe (Ann Params b)) [Ann Decl b]
        (NonEmptyList (Ann Instr b)) (Maybe (Ann Ident b))
  | DFunction (Ann Ident b) (Maybe (Ann Params b)) (Ann Type b) [Ann Decl b]
        (NonEmptyList (Ann Instr b)) (Maybe (Ann Ident b))
  deriving (Eq, Show)

data Champs b = Champs (NonEmptyList (Ann Ident b)) (Ann Type b) deriving (Eq, Show)

data Type b = NoAccess (Ann Ident b) | Access (Ann Ident b) deriving (Eq, Show)

data Params b = Params (NonEmptyList (Ann Param b)) deriving (Eq, Show)

data Param b = Param (NonEmptyList (Ann Ident b)) (Maybe (Ann Mode b)) (Ann Type b) deriving (Eq, Show)

data Mode b = In | InOut deriving (Eq, Show)

data Expr b =
    EInt Integer
  | EChar Char
  | EBool Bool
  | ENull
  -- | EParen (Ann Expr b)
  | EAcces (Ann Acces b)
  | EBinop (Ann Binop b) (Ann Expr b) (Ann Expr b)
  | EUnop (Ann Unop b) (Ann Expr b)
  | ENew (Ann Ident b)
  | ECall (Ann Ident b) (NonEmptyList (Ann Expr b))
  | ECharval (Ann Expr b)
  deriving (Eq, Show)

data Instr b =
    IAssign (Ann Acces b) (Ann Expr b)
  | IIdent (Ann Ident b)
  | ICall (Ann Ident b) (NonEmptyList (Ann Expr b))
  | IReturn (Maybe (Ann Expr b))
  | IBegin (NonEmptyList (Ann Instr b))
  | IIf (Ann Expr b) (NonEmptyList (Ann Instr b))
      [((Ann Expr b), (NonEmptyList (Ann Instr b)))]
       (Maybe (NonEmptyList (Ann Instr b)))
  | IFor (Ann Ident b) Bool (Ann Expr b) (Ann Expr b) (NonEmptyList (Ann Instr b))
  | IWhile (Ann Expr b) (NonEmptyList (Ann Instr b))
  deriving (Eq, Show)

data Binop b = Equal | NotEqual | Lower | LowerEqual | Greater | GreaterEqual
           | Add | Subtract | Multiply | Divide | Rem
           | And | AndThen | Or | OrElse
           deriving (Eq, Show)

data Unop b = Not | Negate deriving (Eq, Show)

data Acces b =
    AccesIdent (Ann Ident b)
  | AccesDot (Ann Expr b) (Ann Ident b)
  deriving (Eq, Show)
