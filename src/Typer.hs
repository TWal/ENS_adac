
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
data TParams = TParams [(String,CType)]
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
    show (TParams ((nm,t) : tps@(_:_))) = nm ++ " : " ++ show t
                                 ++ ", " ++ show tps
    show (TParams [(nm,t)])       = nm ++ " : " ++ show t
    show (TParams [])             = ""
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

findFromContextL :: String -> (String,Map String a) -> Maybe a
findFromContextL s (_,mp) = if M.member s mp then Just $ mp ! s else Nothing
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
    >>= \e -> return $ findFromContext s $ fmap (extract ext) e
mgetC :: (Context -> Map String a) -> String -> String -> Env (Maybe a)
mgetC ext c s = S.get
    >>= \e -> return $ findWithContext c s $ fmap (extract ext) e
mgetL :: (Context -> Map String a) -> String -> Env (Maybe a)
mgetL ext s = S.get
    >>= \(e:^:_) -> return $ findFromContextL s $ extract ext e

getVar  = mget  variables
getVarC = mgetC variables
getVarL = mgetL variables
getFun  = mget  functions
getFunC = mgetC functions
getFunL = mgetL functions
getTpe  = mget  types
getTpeC = mgetC types
getTpeL = mgetL types

-- Shearch on the Pile
hasVar = (CM.liftM $ isJust) . getVar
hasFun = (CM.liftM $ isJust) . getFun
hasTpe = (CM.liftM $ isJust) . getTpe
hasName n = do
    b1 <- hasVar n
    b2 <- hasFun n
    b3 <- hasTpe n
    return $ b1 || b2 || b3
-- Search in one specified context
hasVarC c = (CM.liftM $ isJust) . (getVarC c)
hasFunC c = (CM.liftM $ isJust) . (getFunC c)
hasTpeC c = (CM.liftM $ isJust) . (getTpeC c)
hasNameC c n = do
    b1 <- hasVarC c n
    b2 <- hasFunC c n
    b3 <- hasTpeC c n
    return $ b1 || b2 || b3
-- Search on the top of the Pile
hasVarL v = do
    ((_,mp) :^: _) <- S.get
    return $ M.member v $ variables mp
hasFunL v = do
    ((_,mp) :^: _) <- S.get
    return $ M.member v $ functions mp
hasTpeL v = do
    ((_,mp) :^: _) <- S.get
    return $ M.member v $ types mp
hasNameL n = do
    b1 <- hasVarL n
    b2 <- hasFunL n
    b3 <- hasTpeL n
    return $ b1 || b2 || b3

addVar :: String -> AlexPosn -> CType -> Env ()
addVar s p t = do
    b <- hasNameL s
    if b then lerror p $ s ++ " is already used, can't create variable" else return ()
    e <- S.get
    S.put $ update_head e
          $ extract $ \c -> c { variables = M.insert s t (variables c) }

addFun :: String -> AlexPosn -> Functionnal -> Env ()
addFun s p t = do
    b <- hasNameL s
    if b then lerror p $ s ++ " is already used, can't create functionnal" else return ()
    e <- S.get
    S.put $ update_head e
          $ extract $ \c -> c { functions = M.insert s t (functions c) }

addTpe :: String -> AlexPosn -> Recorded -> Env ()
addTpe s p t = do
    -- TODO handle definition
    b <- hasNameL s
    if b then do
        otp <- getTpeL s
        case otp of
         Nothing          -> return ()
         Just RNotDefined -> return ()
         _                ->lerror p $ s ++ " is already used, can't create type"
    else return ()
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

-- Must not be called on empty list
list_to_non_empty :: [a] -> NonEmptyList a
list_to_non_empty (x : y : xs) = Cons x $ list_to_non_empty $ y : xs
list_to_non_empty [x]          = Last x



type_file :: Fichier AlexPosn -> Env TFichier
type_file (Fichier (Ident name, pos) decls instrs mnm2) = do
    if not b then lerror (snd $ fromJust mnm2) $ " : procedure "
                      ++ name ++ " is renamed " ++ (fromI $ fst $ fromJust mnm2)
             else return ()
    undefined
 where b = isNothing mnm2 || (name == (fromI $ fst $ fromJust mnm2))



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
           ne <- type_expr e -- TODO check if ne is lvalue with compatible type
           CM.foldM (\mp -> \(Ident i,p) -> addv_if mp p i
               $ (RLValue t,Just $ Left ne)) tds
               $ non_empty_to_list ids
       td (DAssign ids (tp, ptp) Nothing, pa) tds = do
           t  <- type_type tp
           CM.foldM (\mp -> \(Ident i,p) -> addv_if mp p i
               $ (RLValue t,Nothing)) tds
               $ non_empty_to_list ids
       td (DProcedure (Ident nm, p) mprs dcls instrs mnm,ppr) tds = do
           let nm2 = case mnm of
                      Just (Ident x,px) -> (x,px)
                      Nothing           -> (nm,ppr)
           if (fst nm2) /= nm then lerror (snd nm2) $ (fst nm2) ++ " is not " ++ nm
                              else return ()
           params <- case mprs of
                      Nothing  -> return []
                      Just prs -> type_params prs
           push_env nm
           CM.forM params $ \(s,t,p) -> addVar s p t -- Adding the parameters to
                                                     -- the environment
           addFun nm ppr $ TProcedure $ TParams $ map drop3 params -- Adding the procedure
           type_decls dcls
           li <- CM.mapM type_instr $ non_empty_to_list instrs
           pop_env
           addf_if tds ppr nm
                   (TProcedure $ TParams $ map drop3 params, list_to_non_empty li)
       td (DFunction (Ident nm, pnm) (Just prs) (rtp, ptp) dcls instrs mnm, pf) tds = do
           let nm2 = case mnm of
                      Just (Ident x,px) -> (x,px)
                      Nothing           -> (nm,pf)
           if (fst nm2) /= nm then lerror (snd nm2) $ (fst nm2) ++ " is not " ++ nm
                              else return ()
           params <- type_params prs
           t <- type_type rtp
           push_env nm
           CM.forM params $ \(s,t,p) -> addVar s p t -- Adding the parameters to
           addFun nm pf $ TFunction (TParams $ map drop3 params) t -- Adding the function
           type_decls dcls
           li <- CM.mapM (type_instr_typed t) $ non_empty_to_list instrs
           pop_env
           addf_if tds pf nm
                   (TFunction (TParams $ map drop3 params) t, list_to_non_empty li)
       td (DFunction (Ident nm, pnm) Nothing (rtp, ptp) dcls instrs mnm, pf) tds = do
           let nm2 = case mnm of
                      Just (Ident x,px) -> (x,px)
                      Nothing           -> (nm,pf)
           if (fst nm2) /= nm then lerror (snd nm2) $ (fst nm2) ++ " is not " ++ nm
                              else return ()
           t <- type_type rtp
           push_env nm
           addVar nm pf $ LValue $ get_class t -- Adding the function
           type_decls dcls
           li <- CM.mapM (type_instr_typed t) $ non_empty_to_list instrs
           pop_env
           addv_if tds pf nm
               (LValue $ get_class t, Just $ Right $ list_to_non_empty li)

       drop3 :: (a,b,c) -> (a,b)
       drop3 (x,y,z) = (x,y)
       addt_if :: TDecls -> AlexPosn -> String -> Recorded -> Env TDecls
       addt_if tds p k e = if M.member k (dtypes tds)
                           then if not $ is_defined $ (dtypes tds) ! k
                                then lerror p $ k ++ " is already declared"
                                else do
                                      updateTpe k p e
                                      return $ tds { dtypes = M.insert k e (dtypes tds) }
                           else do
                                 addTpe k p e
                                 return $ tds { dtypes = M.insert k e (dtypes tds) }
       addv_if :: TDecls -> AlexPosn -> String
               -> (CType,Maybe (Either TPExpr (NonEmptyList TInstr)))
               -> Env TDecls
       addv_if tds p k e = if M.member k (dvars tds)
                           then lerror p $ k ++ " is already declared"
                           else do
                                 addVar k p (fst e)
                                 return $ tds { dvars = M.insert k e (dvars tds) }
       addf_if :: TDecls -> AlexPosn -> String
               -> (Functionnal,NonEmptyList TInstr) -> Env TDecls
       addf_if tds p k e = if M.member k (dfuns tds)
                           then lerror p $ k ++ " is already declared"
                           else do
                                 addFun k p (fst e)
                                 return $ tds { dfuns = M.insert k e (dfuns tds) }



type_champs :: NonEmptyList (Ann Champs AlexPosn)
            -> Env (Map String Typed)
type_champs nl = CM.foldM add_if M.empty l
 where flatten (Champs nl tp) = map (\x -> (x,tp)) $ non_empty_to_list nl
       l = concat $ map (flatten . fst) $ non_empty_to_list nl
       add_if mp ((Ident k, pk), (t, _)) =
           if M.member k mp then lerror pk (k ++ " member declared twice")
           else type_type t >>= (\e -> return $ M.insert k e mp)


type_params :: Ann Params AlexPosn -> Env [(String,CType,AlexPosn)]
type_params (Params prs,pos) = do
    nprs <- mapM tpr mprs
    let dbl = has_double $ map fst3 nprs
    case dbl of
     Nothing -> return nprs
     Just x  -> lerror pos $ x ++ " is used twice in argument list"
 where tpr :: (Ann Ident AlexPosn, Maybe (Ann Mode AlexPosn), Ann Type AlexPosn)
           -> Env (String,CType,AlexPosn)
       tpr ((Ident nm,pnm), md, (tp, ptp)) = do
           t <- type_type tp
           let ctp = case md of
                      Just (In,_)    -> LValue  $ get_class t
                      Just (InOut,_) -> RLValue $ t
                      Nothing        -> LValue  $ get_class t
           return (nm, ctp, pnm)
       fst3 (x,y,z) = x
       mzip :: a -> b -> [c] -> [(c,a,b)]
       mzip x y l = map (\z -> (z,x,y)) l
       from_param (Param nl x y,_) = mzip x y $ non_empty_to_list nl
       mprs = concat $ map from_param $ non_empty_to_list prs
       has_double :: Eq a => [a] -> Maybe a
       has_double (x:xs) = if elem x xs then Just x else has_double xs
       has_double []     = Nothing



type_expr :: Ann Expr AlexPosn -> Env TPExpr
type_expr _ = undefined



type_access :: Acces AlexPosn -> Env TAccess
type_access _ = undefined



type_instr :: Ann Instr AlexPosn -> Env TInstr
type_instr _ = undefined

type_instr_typed :: Typed -> Ann Instr AlexPosn -> Env TInstr
type_instr_typed _ _ = undefined

