
module Typer where
import           AST
import           Lexer
import           Data.Map      (Map, (!))
import qualified Data.Map      as M
import           Data.Maybe
import           Data.List
import qualified Control.Monad as CM
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
           | TRecord String
           | TAccess String
           | Typenull
instance Eq Typed where
    TInteger   == TInteger   = True
    TCharacter == TCharacter = True
    TBoolean   == TBoolean   = True
    Typenull   == Typenull   = True
    TRecord n1 == TRecord n2 = n1 == n2
    TAccess n1 == TAccess n2 = n1 == n2
    _          == _          = False
data Functionnal = TFunction TParams Typed | TProcedure TParams
data TParams = TParams (NonEmptyList (String,CType))
data Recorded = Record (Map String Typed) | RAccess String | RNotDefined
is_defined :: Recorded -> Bool
is_defined RNotDefined = False
is_defined _           = True

data CType = RValue Typed | RLValue Typed | LValue TypeClass
data Context = Context
    { variables :: Map String CType
    , functions :: Map String Functionnal
    , types     :: Map String Recorded
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
    show TInteger    = "Integer"
    show TCharacter  = "Character"
    show TBoolean    = "Bool"
    show (TRecord n) = "Record " ++ n
    show (TAccess n) = "Access " ++ n
    show Typenull    = "Typenull"
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

-- TODO : fail if adding an already present variable, functions or declared record
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

addTpe :: String -> Recorded -> Env ()
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
    { dtypes :: Map String Recorded
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
       tpf (TAccess _) = Right ()
       tpf t           = fail $ show t ++ " has no null value"

num :: TypeClass
num = TypeClass tpf "numeric"
 where tpf TInteger = Right ()
       tpf t        = fail $ "can't convert numeric to " ++ show t

get_class :: Typed -> TypeClass
get_class t = TypeClass (tpf t) $ show t
 where tpf t1 t2 = if t1 == t2 then Right ()
        else fail $ "can't convert " ++ show t1 ++ " to " ++ show t2

-------------- Getting the typing done ----------------------------------------
showPos :: AlexPosn -> String
showPos = show

lerror :: AlexPosn -> String -> Env a
lerror p s = fail $ (showPos p) ++ " : " ++ s

merror :: AlexPosn -> String -> Maybe a -> Env a
merror p s Nothing  = lerror p s
merror p s (Just x) = return x

fromI :: Ident b -> String
fromI (Ident s) = s

non_empty_to_list :: NonEmptyList a -> [a]
non_empty_to_list (Cons x xs) = x : non_empty_to_list xs
non_empty_to_list (Last x)    = [x]



type_file :: Fichier AlexPosn -> Env TFichier
type_file (Fichier (Ident name, pos) decls instrs mnm2) = do
    if not b then lerror (snd $ fromJust mnm2) $ " : procedure "
                      ++ name ++ " is renamed " ++ (fromI $ fst $ fromJust mnm2)
             else return ()
    undefined
 where b = isNothing mnm2 || (name == (fromI $ fst $ fromJust mnm2))



type_param :: Param AlexPosn -> Env TParams
type_param (Param l md b) = undefined



type_type :: Type AlexPosn -> Env Typed
type_type (NoAccess (Ident nm,p)) = case nm of
 "integer"   -> return TInteger
 "character" -> return TCharacter
 "boolean"   -> return TBoolean
 _           -> do
     mt <- getTpe nm
     t  <- merror p ("type " ++ nm ++ " not declared") mt
     if not (is_defined t) then lerror p $ "type " ++ nm ++ " not defined "
     else return $ TRecord nm
type_type (Access (Ident nm,p))   = case nm of
 "integer"   -> lerror p "access only allowed on records, not integer"
 "character" -> lerror p "access only allowed on records, not character"
 "boolean"   -> lerror p "access only allowed on records, not boolean"
 _           -> do
     mt <- getTpe nm
     t  <- merror p ("type " ++ nm ++ " not declared") mt
     return $ TAccess nm



type_decls :: [Ann Decl AlexPosn] -> Env TDecls
type_decls _ = undefined
 where td :: Ann Decl AlexPosn -> TDecls -> Env TDecls
       td (DType (Ident s, p1),p2) tds = addt_if tds p1 s RNotDefined
       td (DAccess (Ident s1, p1) (Ident s2, p2), p3) tds = case s2 of
           "integer"   -> lerror p3 "access only allowed on records, not integer"
           "character" -> lerror p3 "access only allowed on records, not character"
           "boolean"   -> lerror p3 "access only allowed on records, not boolean"
           _           -> do
               mt <- getTpe s2
               t  <- merror p2 ("type " ++ s2 ++ " not declared") mt
               addt_if tds p1 s1 (RAccess s2)
       td (DRecord (Ident nm, pn) lcs, pr) tds = do
           r <- type_champs lcs
           addt_if tds pr nm (Record r)
       td (DAssign ids (tp, ptp) (Just e), pa) tds = do
           t  <- type_type tp
           ne <- type_expr e
           CM.foldM (\mp -> \(Ident i,p) -> addv_if mp p i
               $ (RLValue t,Just $ Left ne)) tds
               $ non_empty_to_list ids

       -- TODO also add to environment
       addt_if :: TDecls -> AlexPosn -> String -> Recorded -> Env TDecls
       addt_if tds p k e = if M.member k (dtypes tds)
                           then lerror p $ k ++ " is already declared" -- Must not fail if RNotDeclared
                           else return $ tds { dtypes = M.insert k e (dtypes tds) }
       addv_if :: TDecls -> AlexPosn -> String
               -> (CType,Maybe (Either TPExpr (NonEmptyList TInstr)))
               -> Env TDecls
       addv_if tds p k e = if M.member k (dvars tds)
                           then lerror p $ k ++ " is already declared"
                           else return $ tds { dvars = M.insert k e (dvars tds) }
       addf_if :: TDecls -> AlexPosn -> String
               -> (Functionnal,NonEmptyList TInstr) -> Env TDecls
       addf_if tds p k e = if M.member k (dfuns tds)
                           then lerror p $ k ++ " is already declared"
                           else return $ tds { dfuns = M.insert k e (dfuns tds) }



type_champs :: NonEmptyList (Ann Champs AlexPosn)
            -> Env (Map String Typed)
type_champs nl = CM.foldM add_if M.empty l
 where flatten (Champs nl tp) = map (\x -> (x,tp)) $ non_empty_to_list nl
       l = concat $ map (flatten . fst) $ non_empty_to_list nl
       add_if mp ((Ident k, pk), (t, _)) =
           if M.member k mp then lerror pk (k ++ " member declared twice")
           else type_type t >>= (\e -> return $ M.insert k e mp)



type_expr :: Ann Expr AlexPosn -> Env TPExpr
type_expr _ = undefined



type_access :: Acces AlexPosn -> Env TAccess
type_access _ = undefined



type_instr :: Instr AlexPosn -> Env TInstr
type_instr _ = undefined

