module CodeGen (genFichier) where
import Typer
import Asm
import AST
import Data.Char (ord)
import Control.Monad
import Control.Monad.State
import           Data.Map      (Map, (!))
import qualified Data.Map      as M
import Data.List (elemIndex)
import Data.Maybe (maybe)

data DeclSizes = DeclSizes
    { tsizes :: Map String Integer
    , voffs  :: Map String Integer
    , frameSize :: Integer
    , argsSize :: Integer
    , fctName :: String
    , nbNestedFor :: Integer
    }

getSize :: [(TDecls, DeclSizes)] -> TId -> Integer
getSize decls (level, name) = tsizes (snd $ decls !! fromIntegral (level-1)) ! name

getOffset :: [(TDecls, DeclSizes)] -> TId -> Integer
getOffset decls (level, name) = voffs (snd $ decls !! fromIntegral (level-1)) ! name

-- Generic function to avoid copy past
getTypedSize' :: Typed -> (Integer -> a) -> (TId -> a) -> a
getTypedSize' t f g =
    case t of
        TInteger -> f 8
        TCharacter -> f 1
        TBoolean -> f 1
        TRecord i -> g i
        TAccess _ -> f 8
        TypeNull -> error "ME DUNNO WAT IZ TEH SIZE OF NULL!!1!"

isRecOrAccess :: CType -> Bool
isRecOrAccess (CType (TAccess _) _ _) = True
isRecOrAccess (CType (TRecord _) _ _) = True
isRecOrAccess _ = False

getParams :: Functionnal -> [(String, CType)]
getParams (TFunction (TParams p) _) = p
getParams (TProcedure (TParams p)) = p

reverse' :: (Foldable t) => t a -> [a]
reverse' = foldl (flip (:)) []

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

mkDeclSizes :: [(TDecls, DeclSizes)] -> Functionnal -> String -> NonEmptyList TInstr -> TDecls -> DeclSizes
mkDeclSizes prev func funcname instrs decls = DeclSizes
    { tsizes = recordedSize
    , voffs = (\(x,_,_) -> x)  offsets
    , frameSize = (\(_,x,_) -> x) offsets + 8*nbNestedFor
    , argsSize = (\(_,_,x) -> x) offsets
    , fctName = funcname
    , nbNestedFor = nbNestedFor
    }
  where
    addMemo :: String -> Integer -> State (Map String Integer) ()
    addMemo str size = state $ \map -> ((), M.insert str size map)

    getMemo :: String -> State (Map String Integer) (Maybe Integer)
    getMemo str = state $ \map -> (M.lookup str map, map)

    getRecordedSize :: TId -> State (Map String Integer) Integer
    getRecordedSize (level, name) =
        -- If it is on a previous level
        if fromIntegral level < length prev then
            return $ getSize prev (level, name)
        else do
            tryMemo <- getMemo name
            case tryMemo of
                -- If it is memoized
                Just size -> return size
                -- Otherwise, compute it
                Nothing -> do
                    size <- case dtypes decls ! name of
                        Record m ->
                            foldM (\acc ty -> do
                                curSize <- getTypedSize ty
                                return $ acc + curSize
                            ) 0 m
                        RNotDefined -> error "MEH"
                        RAlias i -> getRecordedSize i
                        RNType alias -> getTypedSize alias
                    addMemo name size
                    return size

    getTypedSize :: Typed -> State (Map String Integer) Integer
    getTypedSize t = getTypedSize' t return getRecordedSize

    recordedSize :: Map String Integer
    recordedSize = execState (mapM_ getRecordedSize (zip (repeat (fromIntegral $ length prev)) (M.keys (dtypes decls)))) M.empty

    typedSize :: Typed -> Integer
    typedSize t = getTypedSize' t id (\(level, name) ->
        if fromIntegral level < length prev then getSize prev (level, name)
        else recordedSize ! name)

    compOffsets :: [(String, CType)] -> ([(String, Integer)], Integer)
    compOffsets list = (zip (map fst list) (tail sizes), last sizes)
      where sizes = scanl (+) 0 . map ((\(CType t _ _) -> typedSize t) . snd) $ list

    offsets :: (Map String Integer, Integer, Integer)
    offsets = (M.fromList $ (fst varOffs) ++ (map (\(s, i) -> (s, -i-8-8)) . fst $ argsOffs), snd varOffs, snd argsOffs)
      where
        varOffs = compOffsets . map (\(s, (t, _)) -> (s, t)) . M.toList . dvars $ decls
        argsOffs = compOffsets . getParams $ func

    compNbNestedFor :: TInstr -> Integer
    compNbNestedFor (TIAssign _ _) = 0
    compNbNestedFor (TIIdent _) = 0
    compNbNestedFor (TICall _ _) = 0
    compNbNestedFor (TIReturn _) = 0
    compNbNestedFor (TIBegin l) = foldr (max . compNbNestedFor) 0 l
    compNbNestedFor (TIIf l m) = max (foldr (max . compNbNestedFor . TIBegin . snd) 0 l) (maybe 0 (compNbNestedFor . TIBegin) m)
    compNbNestedFor (TIFor _ _ _ _ l) = 1 + compNbNestedFor (TIBegin l)
    compNbNestedFor (TIWhile _ l) = compNbNestedFor (TIBegin l)

    nbNestedFor = compNbNestedFor (TIBegin instrs)


genFichier :: TFichier -> Asm ()
genFichier (TFichier _ decl instrs) =
    genFunction [] "main" (TProcedure (TParams [])) decl instrs

-- Stack when a function is called;
-- some space to give the return value
-- argument n
-- ...
-- argument 1
-- %rbp father
-- return address
-- %rbp of caller
-- local variables...
genFunction :: [(TDecls, DeclSizes)] -> String -> Functionnal -> TDecls -> NonEmptyList TInstr -> Asm ()
genFunction prev name func decl instrs = do
    label (Label name)
    pushq rbp
    movq rsp rbp
    subq fs rsp
    forM_ (M.toList $ dvars decl) (\(s, (_, e)) ->
        maybe (return ()) (\e' -> genInstr (TIAssign (AccessFull (fromIntegral $ length decls, s)) e')) e
        )
    mapM_ genInstr instrs
    movq (int 0) rax
    genInstr (TIReturn Nothing)
    mapM_ (\(s, f) -> uncurry3 (genFunction decls s) f) (M.toList $ dfuns decl)

  where

    fs = frameSize . snd . last $ decls
    decls :: [(TDecls, DeclSizes)]
    decls = prev ++ [(decl, mkDeclSizes prev func name instrs decl)]

    getFctLevel :: String -> Maybe Int
    getFctLevel = flip elemIndex (map (fctName . snd) decls)

    typedSize :: Typed -> Integer
    typedSize t = getTypedSize' t id (getSize decls)

    getOffsets :: Map String Typed -> Map String Integer
    getOffsets vars = M.fromList $ zip (map fst varsList) sizes
      where
        varsList = M.toList vars
        sizes = scanl (+) 0 . map (typedSize . snd) $ varsList

    genAccess :: TAccess -> Asm ()
    genAccess (AccessFull id) =
        if fst id > (fromIntegral $ length decls) then do
            let d = snd . last $ decls
            leaq (Pointer rbp (-((frameSize d) - 8*(nbNestedFor d) + 8*(fst id - (fromIntegral $ length decls))))) rax
        else do
            let off = getOffset decls id
            movq rbp rax
            replicateM_ (length decls - (fromIntegral $ fst id)) (movq (Pointer rax 16) rax)
            leaq (Pointer rax (-off)) rax

    genAccess (AccessPart e str) = do
        genExpr e
        case snd e of
            CType (TAccess i) _ _ -> error "TODO"
            CType (TRecord (level, name)) _ _ ->
                let (Record rec) = dtypes (fst $ decls !! fromIntegral (level-1)) ! name in
                let ofs = getOffsets rec in
                leaq (Pointer rax (ofs ! str)) rax
            _ -> error "MEH."

    doFctCall :: String -> [TPExpr] -> Asm ()
    doFctCall s args = do
        forM_ (reverse' args) (\e -> do
            genExpr e
            pushq rax
            )
        movq rbp rax
        unless (s `elem` ["print_int__", "new_line", "put"]) $ maybe (return ()) (\lev -> replicateM_ (length decls - lev) (movq (Pointer rax 16) rax)) (getFctLevel s)
        pushq rax
        call (Label s)
        popq rax
        forM_ (reverse' args) (\e -> do
            popq rax
            )

    genInstr :: TInstr -> Asm ()

    genInstr (TIAssign acc e) = do
        genExpr e
        pushq rax
        genAccess acc
        popq rbx
        movq rbx (Pointer rax 0)

    genInstr (TIIdent s) = do
        doFctCall s []

    genInstr (TICall s args) = do
        doFctCall s (foldr (:) [] args)

    genInstr (TIReturn Nothing) = do
        addq fs rsp
        popq rbp
        ret

    genInstr (TIReturn (Just e)) = do
        genExpr e
        movq rax (Pointer rbp ((+24) . argsSize . snd . last $ decls))
        genInstr (TIReturn Nothing)

    -- It's useful only for the typer to shadow names
    genInstr (TIBegin le) = do
        mapM_ genInstr le

    genInstr (TIIf lci eci) = do
        endLabel <- getLabel
        forM_ lci (\(cond, instrs) -> do
            nextLabel <- getLabel
            genExpr cond
            testq rax rax
            jz nextLabel
            mapM_ genInstr instrs
            jmp endLabel
            label nextLabel
            )
        forM_ eci (mapM_ genInstr)
        label endLabel

    genInstr (TIFor id rev from to instrs) = do
        if rev then genExpr to
        else genExpr from
        pushq rax
        genAccess (AccessFull id)
        popq rbx
        movq rbx (Pointer rax 0)
        if rev then genExpr from
        else genExpr to
        pushq rax
        bodyLabel <- getLabel
        condLabel <- getLabel
        jmp condLabel
        label bodyLabel
        mapM_ genInstr instrs
        genAccess (AccessFull id)
        movq (Pointer rax 0) rbx
        if rev then decq rbx
        else incq rbx
        movq rbx (Pointer rax 0)
        label condLabel
        genAccess (AccessFull id)
        movq (Pointer rax 0) rax
        popq rbx
        pushq rbx
        cmpq rax rbx
        if rev then jle bodyLabel
        else jge bodyLabel
        popq rbx


    genInstr (TIWhile cond instrs) = do
        bodyLabel <- getLabel
        condLabel <- getLabel
        jmp condLabel
        label bodyLabel
        mapM_ genInstr instrs
        label condLabel
        genExpr cond
        testq rax rax
        jnz bodyLabel


    genExpr :: TPExpr -> Asm ()

    genExpr (TEInt i, _) = do
        movq i rax

    genExpr (TEChar c, _) = do
        movq (int . ord $ c) rax

    genExpr (TEBool b, _) = do
        movq (int $ (if b then 1 else 0)) rax

    genExpr (TENull, _) = do
        movq (int 0) rax

    genExpr (TEAccess a, t) = do
        genAccess a
        unless (isRecOrAccess t) $ movq (Pointer rax 0) rax

    genExpr (TEBinop op e1 e2, _) =
        case op of
            Equal -> evalCond sete
            NotEqual -> evalCond setne
            Lower -> evalCond setl
            LowerEqual -> evalCond setle
            Greater -> evalCond setg
            GreaterEqual -> evalCond setge
            Add -> do
                evalBothExpr
                addq rbx rax
            Subtract -> do
                evalBothExpr
                subq rbx rax
            Multiply -> do
                evalBothExpr
                imulq rbx rax
            Divide -> do
                evalBothExpr
                cqto
                idivq rbx
            Rem -> do
                evalBothExpr
                cqto
                idivq rbx
                movq rdx rax
            And -> do
                evalBothExpr
                andq rbx rax
            AndThen -> error "TODO"
            Or -> do
                evalBothExpr
                orq rbx rax
            OrElse -> error "TODO"
        where
        evalBothExpr :: Asm ()
        evalBothExpr = do
            genExpr e2
            pushq rax
            genExpr e1
            popq rbx
        evalCond :: (Register -> Asm ()) -> Asm ()
        evalCond setcond = do
            evalBothExpr
            cmpq rbx rax
            setcond al
            andq (int 1) rax

    genExpr (TEUnop op e, _) = do
        genExpr e
        case op of
            Not -> do
                xorq (int 1) rax
            Negate -> do
                negq rax

    genExpr (TENew s, _) = error "Not implemented"

    genExpr (TECall s args, _) = do
        pushq rax -- args
        doFctCall s args
        popq rax



    genExpr (TECharval e, _) = do
        genExpr e
