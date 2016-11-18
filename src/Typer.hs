
module Typer where
import AST
import Lexer
import Data.Map (Map)
import qualified Data.Map as M

data Pile a = a :^: Pile a | Bottom a

-------------- General types definitions --------------------------------------
data TypeClass = TypeClass 
    { typify   :: Typed -> Either String ()
    , typename :: String
    }

data Typed = TInteger
           | TCharacter
           | TBoolean
           | TRecord String Recorded
           | TAccess String Recorded
           | Typenull
instance Eq Typed where
    TInteger     == TInteger     = True
    TCharacter   == TCharacter   = True
    TBoolean     == TBoolean     = True
    Typenull     == Typenull     = True
    TRecord n1 _ == TRecord n2 _ = n1 == n2
    TAccess n1 _ == TAccess n2 _ = n1 == n2
    _            == _            = False
data Functionnal = TFunction TParams Typed | TProcedure TParams
data TParams = TParams (NonEmptyList (String,CType))
data Recorded = Record (Map String Typed)

data CType = RValue Typed | RLValue Typed | LValue TypeClass
data Context = Context
    { variables :: Map String CType
    , functions :: Map String Functionnal
    , types     :: Map String (Maybe Typed)
    }
type Env     = Pile (String,Context)

-------------- Outputing ------------------------------------------------------
instance Show TParams where
    show (TParams (Cons (nm,t) tps)) = nm ++ " : " ++ show t
                                    ++ ", " ++ show tps
    show (TParams (Last (nm,t)))     = nm ++ " : " ++ show t
instance Show TypeClass where
    show t = typename t
instance Show CType where
    show (RValue t)  = "out "    ++ show t
    show (LValue t)  = "in "     ++ show t
    show (RLValue t) = "in out " ++ show t
instance Show Typed where
    show TInteger      = "Integer"
    show TCharacter    = "Character"
    show TBoolean      = "Bool"
    show (TRecord n _) = "Record " ++ n
    show (TAccess n _) = "Access " ++ n
    show Typenull      = "Typenull"
instance Show Functionnal where
    show (TFunction tp t) = "(" ++ show tp ++ " -> " ++ show t ++ ")"
    show (TProcedure tp)  = "(" ++ show tp ++ ")"

-------------- Typed AST ------------------------------------------------------
data TFichier = TFichier String TParams TDecls [TInstr]
data TDecls = TDecls
    { dtypes :: Map String Typed
    , dfuns  :: Map String (Functionnal,NonEmptyList TInstr)
    , dvars  :: Map String (CType,Maybe (Either TPExpr (NonEmptyList TInstr)))
    }
type TPExpr = (TExpr,CType)
data TExpr =
    TEInt Integer
  | TEChar Char
  | TEBool Bool
  | TENull
  | TEAccess TAccess
  | TEBinop (Binop ()) TPExpr TPExpr
  | TEUnop (Unop ()) TPExpr
  | TENew String
  | TECall String (NonEmptyList TPExpr)
  | TECharval TPExpr
data TAccess = AccessFull String | AccessPart TPExpr String
data TInstr =
    TIAssign TAccess TPExpr
  | TIIdent String
  | TICall String (NonEmptyList TPExpr)
  | TIReturn (Maybe TPExpr)
  | TIBegin (NonEmptyList TInstr)
  | TIIf (NonEmptyList (TPExpr, NonEmptyList TInstr))
         (Maybe (NonEmptyList TInstr))
  | TIFor String Bool TPExpr TPExpr (NonEmptyList TInstr)
  | TIWhile TPExpr (NonEmptyList TInstr)

-------------- Internals types ------------------------------------------------
null :: TypeClass
null = TypeClass tpf "null"
 where tpf :: Typed -> Either String ()
       tpf (TAccess _ _) = Right ()
       tpf t             = fail $ show t ++ " has no null value"

num :: TypeClass
num = TypeClass tpf "numeric"
 where tpf TInteger = Right ()
       tpf t        = fail $ "can't convert numeric to " ++ show t

get_class :: Typed -> TypeClass
get_class t = TypeClass (tpf t) $ show t
 where tpf t1 t2 = if t1 == t2 then Right ()
        else fail $ "can't convert " ++ show t1 ++ " to " ++ show t2

-------------- Getting the typing done ----------------------------------------
-- TODO : another AST is needed
