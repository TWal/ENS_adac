module Typer ( Typed       (..)
             , Functionnal (..)
             , TParams     (..)
             , Recorded    (..)
             , CType       (..)
             , TFichier    (..)
             , TDecls      (..)
             , TPExpr
             , TExpr       (..)
             , TAccess     (..)
             , TInstr      (..)
             , type_program
             )
    where
import           AST
import           Lexer
import           Data.Map      (Map, (!))
import qualified Data.Map      as M
import           Data.Set      (Set)
import qualified Data.Set      as St
import           Data.Maybe
import           Data.List
import qualified Control.Monad as CM
import qualified Control.Monad.Trans.State.Strict as S
import qualified Debug.Trace as T

data Pile a = a :^: Pile a | Bottom a

-------------- General types definitions --------------------------------------
-- Uniquely identify a type, by its name and level of declaration.
type TId = (Integer,String)
-- Internal types
data Typed = TInteger    -- integer
           | TCharacter  -- character
           | TBoolean    -- boolean
           | TRecord TId -- record, the content of the record
                         -- can be found in the environment,
                         -- using the TId
           | TAccess TId -- Access over a new type
           | TypeNull    -- Special type for the null expression
           deriving (Eq)

is_access :: Typed -> Bool
is_access (TAccess _) = True
is_access _           = False
-- Represent a fonctionnal object, ie a function or procedure
data Functionnal = TFunction TParams Typed | TProcedure TParams
-- Represent the parameters of a function or procedure
data TParams = TParams [(String,CType)]
-- Represent user-defined type
data Recorded = Record (Map String Typed) -- A record, with its members
              | RNotDefined               -- A type declared but not yet defined
              | RAlias TId                -- An alias to another user-defined type
              | RNType Typed              -- An alias to an internal type
is_defined :: Recorded -> Bool
is_defined RNotDefined = False
is_defined _           = True

-- The contextual type of an expression : its internal type and wether it is
-- a r-value or a l-value (or both)
data CType = CType Typed Bool Bool
is_lvalue (CType _ b _) = b
is_rvalue (CType _ _ b) = b
-- A context, ie the declaration of a level (and not those of upper or lower levels).
data Context = Context
    { variables :: Map String CType       -- the contextual types of variables
    , functions :: Map String Functionnal -- the functionnal objects
    , types     :: Map String Recorded    -- the user-defined types
    , declared  :: Set String             -- used to declare identifiers as reserved for variables
                                          -- and functions, shadow types of the same name
    }
-- The environment has as state a pile of contexts (with their level stored) and
-- support string exceptions.
type Env = S.StateT (Pile (Integer,Context)) (Either String)

-------------- Outputing ------------------------------------------------------
instance Show TParams where
    show (TParams ((nm,t) : tps@(_:_))) = nm ++ " : " ++ show t
                                 ++ ", " ++ show tps
    show (TParams [(nm,t)])       = nm ++ " : " ++ show t
    show (TParams [])             = ""
instance Show CType where
    show (CType t False True)  = "in "     ++ show t
    show (CType t True  False) = "out "    ++ show t
    show (CType t True  True)  = "in out " ++ show t
instance Show Typed where
    show TInteger    = "Integer"
    show TCharacter  = "Character"
    show TBoolean    = "Bool"
    show (TRecord n) = "Record " ++ show n
    show (TAccess n) = "Access " ++ show n
    show TypeNull    = "Null"
instance Show Recorded where
    show (Record mp) = "Record " ++ (show $ M.toList mp)
    show RNotDefined = "RNotDefined"
    show (RAlias nm) = "Alias " ++ show nm
    show (RNType t)  = "Type " ++ show t
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

-- Apply a function on the head of a pile
update_head :: Pile a -> (a -> a) -> Pile a
update_head (x :^: xs) f = (f x) :^: xs
update_head (Bottom x) f = Bottom $ f x

-- Apply a function on the second member of a couple
extract :: (a -> b) -> (s,a) -> (s,b)
extract f (x, y) = (x, f y)

-- Give the level of declaration of an identifier in a pile of maps
-- Returns Nothing if the identifier is not defined
_contextOf :: String -> Pile (Integer,Map String a) -> Maybe Integer
_contextOf s ((n,mp) :^: xs) = if M.member s mp then Just n
                                                else _contextOf s xs
_contextOf s (Bottom (n,mp)) = if M.member s mp then Just n
                                                else Nothing
-- Same as above but for the types
contextOf :: String -> Env (Maybe Integer)
contextOf s = do
    e <- S.get
    return $ _contextOf s $ fmap (extract types) e


-- Try to find an identifier in a given context
findFromContextL :: String -> (Integer,Map String a) -> Maybe a
findFromContextL s (_,mp) = if M.member s mp then Just $ mp ! s else Nothing
-- Try to find an identifier in a Pile of maps, while failing if it is found
-- before in any of the others two maps
findFromContext :: String
                -> Pile (Integer,(Map String a,Map String b,Map String c))
                -> Maybe a
findFromContext s ((_,(mp,f1,f2)) :^: xs) = 
    if (M.member s f1) || (M.member s f2) then Nothing
    else if M.member s mp then Just $ mp ! s
    else findFromContext s xs
findFromContext s (Bottom (_,(mp,f1,f2))) = 
    if (M.member s f1) || (M.member s f2) then Nothing
    else if M.member s mp then Just $ mp ! s
    else Nothing

-- Find an identifier in a given context level.
findWithContext :: Integer -> String -> Pile (Integer,Map String a) -> Maybe a
findWithContext c s ((n,mp) :^: xs) =
    if n == c then if M.member s mp then Just $ mp ! s
                   else Nothing
    else findWithContext c s xs
findWithContext c s (Bottom (n,mp)) =
    if n == c then if M.member s mp then Just $ mp ! s
                   else Nothing
    else Nothing

-- Wrap findWithContext in Env monad
mget :: (Context -> (Map String a, Map String b, Map String c))
     -> String -> Env (Maybe a)
mget ext s = S.get
    >>= \e -> return $ findFromContext s $ fmap (extract ext) e
mgetC :: (Context -> Map String a) -> Integer -> String -> Env (Maybe a)
mgetC ext c s = S.get
    >>= \e -> return $ findWithContext c s $ fmap (extract ext) e
mgetL :: (Context -> Map String a) -> String -> Env (Maybe a)
mgetL ext s = S.get
    >>= \e -> return $ findFromContextL s $ extract ext $ phead e

-- get... : try to find the associated variable/type/function, failing if it
-- is shadowed
-- get...C : try to find the associated variable/type/function in a given
-- context level
-- get...L : try to find the associated variable/type/function at the head
-- of the context pile
getVar  = mget  $ \m -> (variables m, types m, functions m)
getVarC = mgetC variables
getVarL = mgetL variables
getFun  = mget  $ \m -> (functions m, variables m, types m)
getFunC = mgetC functions
getFunL = mgetL functions
getTpe  = mget  $ \m -> (types m, variables m, functions m)
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
    e <- S.get
    let (_,mp) = phead e
    return $ M.member v $ variables mp
hasFunL v = do
    e <- S.get
    let (_,mp) = phead e
    return $ M.member v $ functions mp
hasTpeL v = do
    e <- S.get
    let (_,mp) = phead e
    return $ M.member v $ types mp
hasNameL n = do
    b1 <- hasVarL n
    b2 <- hasFunL n
    b3 <- hasTpeL n
    return $ b1 || b2 || b3

-- Search is only local
is_declared :: String -> Env Bool
is_declared n = S.get >>= \e -> return $ St.member n $ declared $ snd $ phead e
declare :: String -> Env ()
declare n = do
    e <- S.get
    S.put $ update_head e
          $ extract $ \c -> c { declared = St.insert n (declared c) }

-- add... : Add a variable/function/type, failing if it is already defined locally
addVar :: String -> Position -> CType -> Env ()
addVar s p t = do
    b <- hasNameL s
    if b then lerror p $ s ++ " is already used, can't create variable" else return ()
    e <- S.get
    S.put $ update_head e
          $ extract $ \c -> c { variables = M.insert s t (variables c) }

addFun :: String -> Position -> Functionnal -> Env ()
addFun s p t = do
    b1 <- hasNameL s
    if b1 then lerror p $ s ++ " is already used, can't create functionnal" else return ()
    e <- S.get
    S.put $ update_head e
          $ extract $ \c -> c { functions = M.insert s t (functions c) }

-- Here, also check if the type name is not already declared
-- In pratice, can not happen
addTpe :: String -> Position -> Recorded -> Env ()
addTpe s p t = do
    b1 <- hasVarL s
    b2 <- hasFunL s
    b3 <- is_declared s
    if b1 || b2 || b3 then
        lerror p $ s ++ " is already used, can't create type"
    else do
        otp <- getTpeL s
        case otp of
         Nothing          -> return ()
         Just RNotDefined -> return ()
         _                ->lerror p $ s ++ " is already used, can't create type"
    e <- S.get
    S.put $ update_head e
          $ extract $ \c -> c { types = M.insert s t (types c) }

empty_context :: Context
empty_context = Context M.empty M.empty M.empty St.empty

-- Handle the pile of contexts
push_env :: Env ()
push_env = do
    e <- S.get
    let (n,_) = phead e
    S.put $ (n+1, empty_context) :^: e

pop_env :: Env (Integer,Context)
pop_env = do
    e <- S.get
    S.put $ ptail e
    return $ phead e

-------------- Typed AST ------------------------------------------------------
data TFichier = TFichier String TDecls (NonEmptyList TInstr)
data TDecls = TDecls
    { dtypes :: Map String Recorded
    , dfuns  :: Map String (Functionnal,TDecls,NonEmptyList TInstr)
    , dvars  :: Map String (CType,Maybe TPExpr)
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
  | TECall String [TPExpr]
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

-------------- Getting the typing done ----------------------------------------
-- Launch an error, with the position
lerror :: Position -> String -> Env a
lerror pos s = fail $ (show pos) ++ ":\n" ++ s

-- Launch an error on Nothing, extract the value otherwise
merror :: Position -> String -> Maybe a -> Env a
merror p s Nothing  = lerror p s
merror p s (Just x) = return x

-- Convert an Ident to a String
fromI :: Ident b -> String
fromI (Ident s) = s

-- Convert a NonEmptyList to the associated list
non_empty_to_list :: NonEmptyList a -> [a]
non_empty_to_list = foldr (:) []

-- Must not be called on empty list
list_to_non_empty :: [a] -> NonEmptyList a
list_to_non_empty (x : y : xs) = Cons x $ list_to_non_empty $ y : xs
list_to_non_empty [x]          = Last x



-- Type the top-level file AST
type_file :: Fichier Position -> Env TFichier
type_file (Fichier (Ident name, pos) decls instrs mnm2) = do
    addFun name pos $ TProcedure $ TParams []
    push_env
    tdcls  <- type_decls decls pos
    tistrs <- CM.mapM type_instr $ non_empty_to_list instrs
    pop_env
    return $ TFichier name tdcls $ list_to_non_empty tistrs



-- Type the string naming a type
type_type :: Type Position -> Env Typed
type_type (NoAccess (Ident nm,p)) = type_lookup Nothing nm p
type_type (Access (Ident nm,p))   = do
    type_get Nothing nm p
    (Just l) <- contextOf nm -- type_get makes sure nm is defined
    return $ TAccess (l,nm)

-- Check if type is declared and return it
-- Context level can be specified
type_get :: Maybe Integer -> String -> Position -> Env Recorded
type_get mc nm p = do
    b <- is_declared nm
    if b then lerror p $ "type " ++ nm ++ "not declared/shadowed" else return ()
    mt <- case mc of
           Just c  -> getTpeC c nm
           Nothing -> getTpe nm
    merror p ("type " ++ nm ++ " not declared/shadowed") mt

-- lookup type recursively over aliases
-- Context level can be specified
type_lookup :: Maybe Integer -> String -> Position -> Env Typed
type_lookup mc nm p = do
    t <- type_get mc nm p
    (Just l) <- contextOf nm -- we're sure t is defined from previous command
                             -- so it can't be Nothing
    case t of
        Record _     -> case mc of
                            Nothing -> return $ TRecord (l,nm)
                            Just c  -> return $ TRecord (c,nm)
        RNotDefined  -> lerror p $ nm ++ " is defined but not declared"
        RAlias (c,n) -> type_lookup (Just c) n p
        RNType t     -> return t


-- Type a list of declarations, adding them to the environment, and
-- storing them in a TDecls structure
type_decls :: [Ann Decl Position] -> Position -> Env TDecls
type_decls dcls p = do
    r <- CM.foldM (flip td) empty_tdecls dcls
    check_declared p
    return r
 where td :: Ann Decl Position -> TDecls -> Env TDecls
       td (DType (Ident s, p1),p2) tds = do
           mt <- getTpe s
           if isNothing mt then return ()
           else if is_defined $ fromJust mt then return ()
           else lerror p2 $ "type " ++ s ++ " is already declared"
           addt_if tds p1 s RNotDefined
       td (DAlias (Ident n, pn) (Ident t,pt),pa) tds = do
           mn <- getTpe n
           if isNothing mn then return ()
           else if is_defined $ fromJust mn then return ()
           else lerror pn $ "type " ++ n ++ " is already declared"
           mt <- getTpe t
           if isNothing mt then lerror pt $ "type " ++ t ++ " is not defined"
           else if not $ is_defined $ fromJust mt
               then lerror pt $ "type " ++ t ++ " declared but nor defined"
           else return ()
           (Just l) <- contextOf t
           addt_if tds pn n (RAlias (l,t))
       td (DAccess (Ident s1, p1) (Ident s2, p2), p3) tds = do
           mt <- getTpe s2
           t  <- merror p2 ("type " ++ s2 ++ " not declared") mt
           if s1 /= s2 then return ()
           else lerror p3 "access type cannot designate itself"
           (Just l) <- contextOf s2
           addt_if tds p1 s1 (RNType $ TAccess (l,s2))
       td (DRecord (Ident nm, pn) lcs, pr) tds = do
           addTpe nm pn RNotDefined
           r <- type_champs lcs
           addt_if tds pr nm (Record r)
       td (DAssign ids (tp, ptp) (Just e@(_,pe)), pa) tds = do
           let lids = non_empty_to_list ids
           CM.mapM_ (declare . \(Ident s,_) -> s) lids
           t  <- type_type tp
           ne@(_,te) <- type_expr e
           case (t,te) of
            (_,CType _ _ False)            -> lerror pe  $ show t  ++ " is not an rvalue"
            (TAccess _,CType TypeNull _ _) -> return ()
            (_, CType t2 _ _)              -> if t == t2 then return ()
                else lerror pe $ show t2 ++ " is not compatible with " ++ show t
           CM.foldM (\mp -> \(Ident i,p) -> addv_if mp p i
               $ (CType t True True,Just ne)) tds lids
       td (DAssign ids (tp, ptp) Nothing, pa) tds = do
           let lids = non_empty_to_list ids
           CM.mapM_ (declare . \(Ident s,_) -> s) lids
           t  <- type_type tp
           CM.foldM (\mp -> \(Ident i,p) -> addv_if mp p i
               $ (CType t True True,Nothing)) tds lids
       td (DProcedure (Ident nm, p) mprs dcls instrs mnm,ppr) tds = do
           check_declared ppr
           let nm2 = case mnm of
                      Just (Ident x,px) -> (x,px)
                      Nothing           -> (nm,ppr)
           if (fst nm2) /= nm then lerror (snd nm2) $ (fst nm2) ++ " is not " ++ nm
                              else return ()
           params <- case mprs of
                      Nothing  -> return []
                      Just prs -> type_params prs
           addFun nm ppr $ TProcedure $ TParams $ map drop3 params -- Adding the procedure
           push_env
           CM.forM params $ \(s,t,p) -> addVar s p t -- Adding the parameters to
                                                     -- the environment
           ntds <- type_decls dcls ppr
           li <- CM.mapM type_instr $ non_empty_to_list instrs
           pop_env
           addf_if tds ppr nm
                   (TProcedure $ TParams $ map drop3 params, ntds, 
                        list_to_non_empty li)
       td (DFunction (Ident nm, pnm) (Just prs) (rtp, ptp) dcls instrs mnm, pf) tds = do
           check_declared pf
           let nm2 = case mnm of
                      Just (Ident x,px) -> (x,px)
                      Nothing           -> (nm,pf)
           if (fst nm2) /= nm then lerror (snd nm2) $ (fst nm2) ++ " is not " ++ nm
                              else return ()
           params <- type_params prs
           t <- type_type rtp
           addFun nm pf $ TFunction (TParams $ map drop3 params) t -- Adding the function
           push_env
           CM.forM params $ \(s,t,p) -> addVar s p t -- Adding the parameters to
           ntds <- type_decls dcls pf
           li <- type_instr_typed t instrs
           pop_env
           addf_if tds pf nm
                   (TFunction (TParams $ map drop3 params) t, ntds, li)
       td (DFunction (Ident nm, pnm) Nothing (rtp, ptp) dcls instrs mnm, pf) tds = do
           check_declared pf
           let nm2 = case mnm of
                      Just (Ident x,px) -> (x,px)
                      Nothing           -> (nm,pf)
           if (fst nm2) /= nm then lerror (snd nm2) $ (fst nm2) ++ " is not " ++ nm
                              else return ()
           t <- type_type rtp
           addFun nm pf $ TFunction (TParams []) t -- Adding the function
           push_env
           ntds <- type_decls dcls pf
           li <- type_instr_typed t instrs
           pop_env
           addf_if tds pf nm
               (TFunction (TParams []) t, ntds, li)

       check_declared :: Position -> Env ()
       check_declared p = do
           e <- S.get
           let nm = M.filter (not . is_defined) $ types $ snd $ phead e
           if not $ M.null nm then lerror p $ "type " ++ show (head $ M.elems nm)
                                           ++ " declared but not defined"
           else return ()
       empty_tdecls = TDecls M.empty M.empty M.empty
       drop3 :: (a,b,c) -> (a,b)
       drop3 (x,y,z) = (x,y)
       addt_if :: TDecls -> Position -> String -> Recorded -> Env TDecls
       addt_if tds p k e = if M.member k (dtypes tds)
                           then if is_defined $ (dtypes tds) ! k
                                then lerror p $ k ++ " is already defined"
                                else do
                                      addTpe k p e
                                      return $ tds { dtypes = M.insert k e (dtypes tds) }
                           else do
                                 addTpe k p e
                                 return $ tds { dtypes = M.insert k e (dtypes tds) }
       addv_if :: TDecls -> Position -> String
               -> (CType,Maybe TPExpr)
               -> Env TDecls
       addv_if tds p k e = if M.member k (dvars tds)
                           then lerror p $ k ++ " is already declared"
                           else do
                                 addVar k p (fst e)
                                 return $ tds { dvars = M.insert k e (dvars tds) }
       addf_if :: TDecls -> Position -> String
               -> (Functionnal,TDecls,NonEmptyList TInstr) -> Env TDecls
       addf_if tds p k e = if M.member k (dfuns tds)
                           then lerror p $ k ++ " is already declared"
                           else do
                                 -- The functionnal has already been added
                                 return $ tds { dfuns = M.insert k e (dfuns tds) }



-- Type the members of a new record
type_champs :: NonEmptyList (Ann Champs Position)
            -> Env (Map String Typed)
type_champs nl = CM.foldM add_if M.empty l
 where flatten (Champs nl tp) = map (\x -> (x,tp)) $ non_empty_to_list nl
       l = concat $ map (flatten . fst) $ non_empty_to_list nl
       add_if mp ((Ident k, pk), (t, _)) =
           if k == "all" then lerror pk "all is a reserved record member name"
           else if M.member k mp then lerror pk (k ++ " member declared twice")
           else type_type t >>= (\e -> return $ M.insert k e mp)


-- Type the list of parameters of a function/procedure
type_params :: Ann Params Position -> Env [(String,CType,Position)]
type_params (Params prs,pos) = do
    nprs <- mapM tpr mprs
    let dbl = has_double $ map fst3 nprs
    case dbl of
     Nothing -> return nprs
     Just x  -> lerror pos $ x ++ " is used twice in argument list"
 where tpr :: (Ann Ident Position, Maybe (Ann Mode Position), Ann Type Position)
           -> Env (String,CType,Position)
       tpr ((Ident nm,pnm), md, (tp, ptp)) = do
           declare nm
           t <- type_type tp
           let ctp = case md of
                      Just (In,_)    -> CType t False True
                      Just (InOut,_) -> CType t True  True
                      Nothing        -> CType t False True
           return (nm, ctp, pnm)
       fst3 (x,y,z) = x
       mzip :: a -> b -> [c] -> [(c,a,b)]
       mzip x y l = map (\z -> (z,x,y)) l
       from_param (Param nl x y,_) = mzip x y $ non_empty_to_list nl
       mprs = concat $ map from_param $ non_empty_to_list prs
       has_double :: Eq a => [a] -> Maybe a
       has_double (x:xs) = if elem x xs then Just x else has_double xs
       has_double []     = Nothing



-- Type an expression
type_expr :: Ann Expr Position -> Env TPExpr
type_expr (EInt i,_)  = return (TEInt i,  CType TInteger   False True)
type_expr (EChar c,_) = return (TEChar c, CType TCharacter False True)
type_expr (EBool b,_) = return (TEBool b, CType TBoolean   False True)
type_expr (ENull, _)  = return (TENull,   CType TypeNull   False True)
type_expr (EAcces (a,pa),_) = type_access a
type_expr (EBinop (b,pb) e1@(_,pe1) e2@(_,pe2), peb) = do
    ne1@(_,cte1@(CType te1 _ b1)) <- type_expr e1
    ne2@(_,cte2@(CType te2 _ b2)) <- type_expr e2
    if not b1 then lerror pe1 "is not an rvalue"
    else if not b2 then lerror pe2 "is not an rvalue"
    else return ()
    if b `elem` [Add, Subtract, Multiply, Divide, Rem,
                 Lower, LowerEqual, Greater, GreaterEqual]
        then if te1 /= TInteger
                 then lerror pe1 $ "expected integer, got " ++ show te1
             else if te2 /= TInteger
                 then lerror pe2 $ "expected integer, got " ++ show te2
             else if b `elem` [Add, Subtract, Multiply, Divide, Rem]
                  then return (TEBinop (cvbnp b) ne1 ne2, CType TInteger False True)
                  else return (TEBinop (cvbnp b) ne1 ne2, CType TBoolean False True)
    else do
      if b `elem` [And, AndThen, Or, OrElse]
        then if te1 /= TBoolean
                 then lerror pe1 $ "expected boolean, got " ++ show te1
             else if te2 /= TBoolean
                 then lerror pe2 $ "expected boolean, got " ++ show te2
             else return ()
      else    if is_access te1   && te2 == TypeNull then return ()
         else if te1 == TypeNull && is_access te2   then return ()
         else if te1 == te2                         then return ()
         else lerror peb $ "can't compare " ++ show te1 ++ " and " ++ show te2
      return (TEBinop (cvbnp b) ne1 ne2, CType TBoolean False True)
 where cvbnp :: Binop Position -> Binop ()
       cvbnp = fmap (const ())

type_expr (EUnop (Not, pu) e@(_,pe), pun) = do
    ne@(_,cte@(CType te _ b)) <- type_expr e
    if not b then lerror pe "is not rvalue" else return ()
    if te /= TBoolean then lerror pe $ "expecting boolean, got " ++ show te
    else return (TEUnop Not ne, CType TBoolean False True)
type_expr (EUnop (Negate, pu) e@(_,pe), pun) = do
    ne@(_,cte@(CType te _ b)) <- type_expr e
    if not b then lerror pe "is not rvalue" else return ()
    if te /= TInteger then lerror pe $ "expecting integer, got " ++ show te
    else return (TEUnop Negate ne, CType TInteger False True)
type_expr (ENew (Ident r,pr), pe) = do
    mt <- getTpe r
    case mt of
        Nothing         -> lerror pr $ "type " ++ r ++ " is not defined"
        Just (Record _) -> return ()
        Just t2         -> lerror pr $ "expected defined record"
    (Just l) <- contextOf r
    return (TENew r, CType (TAccess (l,r)) False True)
type_expr (ECharval e@(_,pe), pc) = do
    ne@(_,(CType te _ b)) <- type_expr e
    if not b then lerror pe "is not rvalue" else return ()
    if te /= TInteger then lerror pe $ "expecting character, got " ++ show te
    else return (TECharval ne, CType TCharacter False True)
type_expr (ECall (Ident f,pf) params, pc) = do
    nf <- getFun f
    case nf of
     Nothing             -> lerror pf $ f ++ " is not declared as a function"
     Just (TProcedure _) -> lerror pc $ f ++ " is a procedure, not a function"
     Just (TFunction (TParams prs) tp) -> do
          let cprs = non_empty_to_list params
          if length cprs == length prs then return ()
          else lerror pc $ "function " ++ f ++ " expects " ++ show (length prs)
                        ++ " arguments, " ++ show (length cprs) ++ " given"
          tprs <- CM.mapM cmppr $ zip cprs prs
          return (TECall f tprs, CType tp False True)

-- Check if an expression has the expected type
cmppr :: (Ann Expr Position, (String,CType)) -> Env TPExpr
cmppr (e@(_,pe),(s,CType t o i)) = do
    ne@(_,(CType te b1 b2)) <- type_expr e
    if i && not b2 then lerror pe "expecting a rvalue for in parameter"
    else if o && not b1 then lerror pe "expecting a lvalue for out parameter"
    else return ()
    if is_access t && te == TypeNull then return ne
    else if t == te                  then return ne
    else lerror pe $ "expected " ++ show t ++ ", got " ++ show te ++ " for " ++ s



-- Type an acces (TODO type_access -> type_acces)
type_access :: Acces Position -> Env TPExpr
type_access (AccesIdent (Ident s,p)) = do
    v <- getVar s
    case v of
     Nothing -> do f <- getFun s
                   case f of
                    Nothing -> lerror p $ s ++ " is not defined"
                    Just (TProcedure _) -> lerror p $ s ++ " is a procedure"
                    Just (TFunction (TParams l@(_:_)) _) ->
                        lerror p $ s ++ "expected " ++ show (length l)
                                ++ "parameters, 0 given"
                    Just (TFunction (TParams []) t) ->
                        return (TECall s [], CType t False True)
     Just t  -> return (TEAccess $ AccessFull s, t)
type_access (AccesDot ie@(_,pe) (Ident f, pf)) = do
    te@(_,tpe) <- type_expr ie
    rtp <- case tpe of
     CType (TAccess (l,n)) _  b2 -> if f == "all"
                                     then type_lookup (Just l) n pe >>= \x -> return $ CType x True b2
                                     else get_sub (l,n) pe f pf     >>= \x -> return $ CType x True b2
     CType (TRecord s)     b1 b2 -> get_sub s pe f pf >>= \x -> return $ CType x b1   b2
     _                       -> lerror pe $ "expression does not evaluate to a record"
    return (TEAccess $ AccessPart te f, rtp)
 where get_sub :: TId -> Position -> String -> Position -> Env Typed
       get_sub (c,s) ps f pf = do
        t <- getTpeC c s
        case t of
         Nothing -> lerror pe $ "expression has unvalid " ++ s ++ " type"
         Just (Record mp) -> if not $ M.member f mp
            then lerror pf $ "record " ++ s ++ " has no member " ++ f
            else return $ mp ! f
         Just (RAlias nm)               -> get_sub nm ps f pf
         Just (RNType (TRecord nm))     -> get_sub nm ps f pf
         Just (RNType nt)               -> lerror ps $ show nt ++ " has no member " ++ f
         Just RNotDefined               -> lerror pe $ s ++ " is declared but not defined"



-- Type an instruction
type_instr :: Ann Instr Position -> Env TInstr
type_instr = type_instr_g Nothing

-- Type an instruction, which can either have no type or an expected
-- one (used when typing returns)
type_instr_g :: Maybe Typed -> Ann Instr Position -> Env TInstr
type_instr_g _ (IAssign (a, pa) e@(_, pe), pia) = do
    ne@(_, CType te _ b) <- type_expr e
    if not b then lerror pe $ "expecting a rvalue" else return ()
    (ea,CType ta b2 _) <- type_access a
    if not b2 then lerror pa $ "expecting a lvalue" else return ()
    let (TEAccess na) = ea
    if is_access ta && te == TypeNull then return $ TIAssign na ne
    else if ta == te then return $ TIAssign na ne
    else lerror pia $ "cannot assign a " ++ show te ++ " to a " ++ show ta
type_instr_g _ (IIdent (Ident f, pf), pii) = do
    mv <- getFun f
    case mv of
     Nothing -> lerror pf $ "no known procedure " ++ f
     Just (TFunction _ _) -> lerror pf $ f ++ " is a function, not a procedure"
     Just (TProcedure (TParams [])) -> return $ TIIdent f
     Just (TProcedure (TParams _))  -> lerror pii $ "procedure " ++ f
                                                 ++ " expects parameters"
type_instr_g _ (ICall (Ident f, pf) params, pic) = do
    mv <- getFun f
    case mv of
     Nothing -> lerror pf $ "no known procedure " ++ f
     Just (TFunction _ _) -> lerror pf $ f ++ " is a function, not a procedure"
     Just (TProcedure (TParams []))  -> lerror pic $ "procedure " ++ f
                                                  ++ " expects no parameters"
     Just (TProcedure (TParams rpr)) -> do
         let lpr = non_empty_to_list params
         if length rpr == length lpr then return ()
         else lerror pic $ f ++ " expects " ++ show (length rpr) ++ " parameters ,"
                        ++ show (length lpr) ++ " given"
         mpr <- CM.mapM cmppr $ zip lpr rpr
         return $ TICall f $ list_to_non_empty mpr
type_instr_g Nothing  (IReturn Nothing,p) = return $ TIReturn Nothing
type_instr_g (Just t) (IReturn Nothing,p) =
    lerror p $ "expecting " ++ show t ++ " in return, got a unit"
type_instr_g Nothing  (IReturn _,p) =
    lerror p "cannot return in unit instruction"
type_instr_g (Just t) (IReturn (Just me),pe) = do
    ne@(_, CType te _ b) <- type_expr me
    if not b then lerror pe "expecting a rvalue"
    else if is_access t && te == TypeNull then return ()
    else if t == te then return ()
    else lerror pe $ "expecting a " ++ show t ++ ", got a " ++ show te
    return $ TIReturn $ Just ne
type_instr_g t (IBegin instrs, pib) = do
    li <- mapM (type_instr_g t) $ non_empty_to_list instrs
    return $ TIBegin $ list_to_non_empty li
type_instr_g t (IIf e l lxs ml, pif) = do
    (ne,nl) <- type_if (e,l)
    nlxs    <- CM.mapM type_if lxs
    nml     <- case ml of
                Nothing -> return Nothing
                Just l2 -> do
                    nl2 <- CM.mapM (type_instr_g t) $ non_empty_to_list l2
                    return $ Just $ list_to_non_empty nl2
    return $ TIIf (list_to_non_empty ((ne,nl) : nlxs)) nml
 where type_if :: (Ann Expr Position, NonEmptyList (Ann Instr Position))
               -> Env (TPExpr, NonEmptyList TInstr)
       type_if (e@(_,pe), l) = do
           ne@(_,CType te _ b) <- type_expr e
           if not b then lerror pe "expecting a rvalue" else return ()
           if te == TBoolean then return ()
           else lerror pe $ "expecting a boolean, got a " ++ show te
           li <- CM.mapM (type_instr_g t) $ non_empty_to_list l
           return (ne, list_to_non_empty li)
type_instr_g t (IFor (Ident v,pv) b e1@(_,pe1) e2@(_,pe2) instrs, pif) = do
    ne1@(_, CType te1 _ b1) <- type_expr e1
    ne2@(_, CType te2 _ b2) <- type_expr e2
    if not b1 then lerror pe1 "expecting a rvalue"
    else if not b2 then lerror pe2 "expecting a rvalue"
    else return ()
    if te1 /= TInteger then lerror pe1 $ "expecting a integer, got a " ++ show te1
    else if te2 /= TInteger then lerror pe2 $ "expecting a integer, got a" ++ show te2
    else return ()
    push_env
    addVar v pv $ CType TInteger False True
    li <- CM.mapM (type_instr_g t) $ non_empty_to_list instrs
    pop_env
    return $ TIFor v b ne1 ne2 $ list_to_non_empty li
type_instr_g t (IWhile e@(_, pe) instrs, pw) = do
    ne@(_,CType te _ b) <- type_expr e
    if not b then lerror pe "expecting a rvalue"
    else if te /= TBoolean then lerror pe $ "expecting a boolean, got a " ++ show te
    else return ()
    li <- CM.mapM (type_instr_g t) $ non_empty_to_list instrs
    return $ TIWhile ne $ list_to_non_empty li

-- Type an instruction list, with a return type expected (the instruction list of a function)
-- Will fail if control can reach the end of the list of instructions without encountering
-- returns
type_instr_typed :: Typed -> NonEmptyList (Ann Instr Position) -> Env (NonEmptyList TInstr)
type_instr_typed t l = do
    (r,m) <- type_ityped t l
    case m of
     Nothing -> return r
     Just p  -> lerror p "control reach end of function"

-- Same as above, but instead of raising an instruction on failure will
-- return the posistion of the failure.
-- Will remove unreachable instructions, but still type them
type_ityped :: Typed -> NonEmptyList (Ann Instr Position)
            -> Env (NonEmptyList TInstr, Maybe Position)
type_ityped t (Cons i@(IAssign _ _,_) is) = do
    ni      <- type_instr_g (Just t) i
    (nis,p) <- type_ityped t is
    return (Cons ni nis,p)
type_ityped t (Cons i@(IIdent _,_) is) = do
    ni      <- type_instr_g (Just t) i
    (nis,p) <- type_ityped t is
    return (Cons ni nis, p)
type_ityped t (Cons i@(ICall _ _,_) is) = do
    ni      <- type_instr_g (Just t) i
    (nis,p) <- type_ityped t is
    return (Cons ni nis, p)
type_ityped t (Cons (IReturn (Just me),pe) is) = do
    ne@(_, CType te _ b) <- type_expr me
    if not b then lerror pe "expecting a rvalue"
    else if is_access t && te == TypeNull then return ()
    else if t == te then return ()
    else lerror pe $ "expecting a " ++ show t ++ ", got a " ++ show te
    type_ityped t is -- Discard following instructions, they can't be reached
                     -- Yet type them to raise errors
    return (Last $ TIReturn $ Just ne, Nothing)
type_ityped t (Cons (IReturn Nothing, p) _) =
    lerror p $ "expecting " ++ show t ++ " in return, got a unit"
type_ityped t (Cons (IBegin iis,pi) is) = do
    (niis,m) <- type_ityped t iis
    (nis,np) <- type_ityped t is
    case m of
     Nothing -> return (Last $ TIBegin niis, Nothing)
     Just _  -> return (Cons (TIBegin niis) nis, np)
type_ityped t (Cons i@(IIf _ _ _ Nothing,_) is) = do
    ni      <- type_instr_g (Just t) i
    (nis,p) <- type_ityped t is
    return (Cons ni nis,p)
type_ityped t (Cons (IIf e l lxs (Just l2), pif) is) = do
    ((ne,nl),p1) <- type_if (e,l)
    nlxs         <- CM.mapM type_if lxs
    (nml,p2)     <- type_ityped t l2
    -- p :: Maybe (Maybe Position), but we don't as we discard the value if
    -- it's a Just
    let p = listToMaybe $ filter isJust $ p1 : p2 : map snd nlxs
    case p of
     Nothing -> return ( Last (TIIf (list_to_non_empty ((ne,nl) : (map fst nlxs)))
                                    (Just nml))
                       , Nothing)
     Just _ -> do
         (nis,np) <- type_ityped t is
         return ( Cons (TIIf (list_to_non_empty ((ne,nl) : (map fst nlxs)))
                             (Just nml))
                  nis
                , np)
 where type_if :: (Ann Expr Position, NonEmptyList (Ann Instr Position))
               -> Env ((TPExpr, NonEmptyList TInstr), Maybe Position)
       type_if (e@(_,pe), l) = do
           ne@(_,CType te _ b) <- type_expr e
           if not b then lerror pe "expecting a rvalue" else return ()
           if te == TBoolean then return ()
           else lerror pe $ "expecting a boolean, got a " ++ show te
           (li,p) <- type_ityped t l
           return ((ne, li), p)
type_ityped t (Cons i@(IFor _ _ _ _ _,pf) is) = do
    ni      <- type_instr_g (Just t) i
    (nis,p) <- type_ityped t is
    return (Cons ni nis, p)
type_ityped t (Cons i@(IWhile _ _,pw) is) = do
    ni      <- type_instr_g (Just t) i
    (nis,p) <- type_ityped t is
    return (Cons ni nis, p)
-- Only a valid return, an ending block or an ending if can go last
type_ityped t (Last (IReturn Nothing,p)) =
    lerror p $ "expecting " ++ show t ++ " in return, got a unit"
type_ityped t (Last (IReturn (Just me),pe)) = do
    ne@(_, CType te _ b) <- type_expr me
    if not b then lerror pe "expecting a rvalue"
    else if is_access t && te == TypeNull then return ()
    else if t == te then return ()
    else lerror pe $ "expecting a " ++ show t ++ ", got a " ++ show te
    return (Last $ TIReturn $ Just ne, Nothing)
type_ityped t (Last (IBegin iis,pi)) = do
    (niis,m) <- type_ityped t iis
    return (Last $ TIBegin niis, m)
type_ityped t (Last (IIf e l lxs (Just l2), pif)) = do
    ((ne,nl),p1) <- type_if (e,l)
    nlxs         <- CM.mapM type_if lxs
    (nml,p2)     <- type_ityped t l2
    -- p :: Maybe (Maybe Position), but we don't as we discard the value if
    -- it's a Just
    let p = listToMaybe $ filter isJust $ p1 : p2 : map snd nlxs
    case p of
     Nothing -> return ( Last (TIIf (list_to_non_empty ((ne,nl) : (map fst nlxs)))
                                    (Just nml))
                       , Nothing)
     Just np -> lerror (fromJust np) "control reach end of function"
 where type_if :: (Ann Expr Position, NonEmptyList (Ann Instr Position))
               -> Env ((TPExpr, NonEmptyList TInstr), Maybe Position)
       type_if (e@(_,pe), l) = do
           ne@(_,CType te _ b) <- type_expr e
           if not b then lerror pe "expecting a rvalue" else return ()
           if te == TBoolean then return ()
           else lerror pe $ "expecting a boolean, got a " ++ show te
           (li,p) <- type_ityped t l
           return ((ne, li), p)
type_ityped t (Last i@(IFor _ _ _ _ _,pf)) = do
    ni      <- type_instr_g (Just t) i
    return (Last ni, Just pf)
type_ityped t (Last i@(IWhile _ _,pw)) = do
    ni      <- type_instr_g (Just t) i
    return (Last ni, Just pw)
type_ityped t (Last i@(IAssign _ _,pa)) = do
    ni      <- type_instr_g (Just t) i
    return (Last ni, Just pa)
type_ityped t (Last i@(IIdent _,pi)) = do
    ni      <- type_instr_g (Just t) i
    return (Last ni, Just pi)
type_ityped t (Last i@(ICall _ _,pc)) = do
    ni      <- type_instr_g (Just t) i
    return (Last ni, Just pc)
type_ityped t (Last i@(IIf _ _ _ Nothing,pi)) = do
    ni      <- type_instr_g (Just t) i
    return (Last ni, Just pi)



type_program :: Fichier Position -> Either String TFichier
type_program f = S.evalStateT (type_file f) (Bottom (0, e))
 where e = Context vars funs tps St.empty
       funs = M.fromList
              [ ("put", TProcedure $ TParams [("o", CType TCharacter False True)])
              , ("new_line", TProcedure $ TParams [])
              ]
       tps  = M.fromList
              [ ("integer",   RNType TInteger)
              , ("character", RNType TCharacter)
              , ("boolean",   RNType TBoolean)
              ]
       vars = M.empty

