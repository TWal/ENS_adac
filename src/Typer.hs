
module Typer where
import AST
import Lexer
import Data.Map (Map, (!))
import qualified Data.Map as M
import qualified Control.Monad.Trans.State.Strict as S

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
type Env = S.StateT (Pile (String,Context)) (Either String)

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

-------------- Handling environment -------------------------------------------
instance Functor Pile where
    fmap f (x :^: xs) = f x :^: fmap f xs
    fmap f (Bottom x) = Bottom $ f x

phead :: Pile a -> a
phead (x :^:  _) = x
phead (Bottom x) = x

ptail :: Pile a -> Pile a
ptail (_ :^: xs) = xs
ptail bx         = bx -- Maybe it should fail

update_head :: Pile a -> (a -> a) -> Pile a
update_head (x :^: xs) f = (f x) :^: xs
update_head (Bottom x) f = Bottom $ f x

extract :: (a -> b) -> (s,a) -> (s,b)
extract f (x, y) = (x, f y)

findFromContext :: String -> Pile (String,Map String a) -> Maybe a
findFromContext s ((_,mp) :^: xs) = if M.member s mp then Just $ mp ! s
                                                     else findFromContext s xs
findFromContext s (Bottom (_,mp)) = if M.member s mp then Just $ mp ! s
                                                     else Nothing

findWithContext :: String -> String -> Pile (String,Map String a) -> Maybe a
findWithContext c s ((n,mp) :^: xs) =
    if n == c then if M.member s mp then Just $ mp ! s
                   else Nothing
    else findWithContext c s xs
findWithContext c s (Bottom (n,mp)) =
    if n == c then if M.member s mp then Just $ mp ! s
                   else Nothing
    else Nothing

mget :: (Context -> Map String a) -> String -> Env (Maybe a)
mget ext s = S.get
    >>= (\e -> return $ findFromContext s $ fmap (extract ext) e)
mgetC :: (Context -> Map String a) -> String -> String -> Env (Maybe a)
mgetC ext c s = S.get
    >>= (\e -> return $ findWithContext c s $ fmap (extract ext) e)

getVar  = mget  variables
getVarC = mgetC variables
getFun  = mget  functions
getFunC = mgetC functions
getTpe  = mget  types
getTpeC = mgetC types

addVar :: String -> CType -> Env ()
addVar s t = do
    e <- S.get
    S.put $ update_head e
          $ extract $ \c -> c { variables = M.insert s t (variables c) }

addFun :: String -> Functionnal -> Env ()
addFun s t = do
    e <- S.get
    S.put $ update_head e
          $ extract $ \c -> c { functions = M.insert s t (functions c) }

addTpe :: String -> Maybe Typed -> Env ()
addTpe s t = do
    e <- S.get
    S.put $ update_head e
          $ extract $ \c -> c { types = M.insert s t (types c) }
updateTpe = addTpe

empty_context :: Context
empty_context = Context M.empty M.empty M.empty

push_env :: String -> Env ()
push_env s = do
    e <- S.get
    S.put $ (s, empty_context) :^: e

pop_env :: Env (String,Context)
pop_env = do
    e <- S.get
    S.put $ ptail e
    return $ phead e

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
type_file :: Fichier b -> Env TFichier
type_file (Fichier (name, pos) decls instrs mnm2) = _

type_decls :: [Ann Decl b] -> Env TDecls
type_decls _ = _

type_expr :: Expr b -> Env TPExpr
type_expr _ = _

type_access :: Acces b -> Env TAccess
type_access _ = _

type_instr :: Instr b -> Env TInstr
type_instr _ = _

