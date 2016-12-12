module CodeGen (genFichier) where
import Typer
import Asm
import AST
import Data.Char (ord)
import Control.Monad
import Control.Monad.State
import           Data.Map      (Map, (!))
import qualified Data.Map      as M

data DeclSizes = DeclSizes
    { tsizes :: Map String Integer
    , voffs  :: Map String Integer
    , frameSize :: Integer
    , argsSize :: Integer
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

mkDeclSizes :: [(TDecls, DeclSizes)] -> Functionnal -> TDecls -> DeclSizes
mkDeclSizes prev func decls = DeclSizes
    { tsizes = recordedSize
    , voffs = (\(x,_,_) -> x)  offsets
    , frameSize = (\(_,x,_) -> x) offsets
    , argsSize = (\(_,_,x) -> x) offsets
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
    offsets = (M.fromList $ (fst varOffs) ++ (map (\(s, i) -> (s, -i-8-8-8)) . fst $ argsOffs), snd varOffs, snd argsOffs)
      where
        varOffs = compOffsets . map (\(s, (t, _)) -> (s, t)) . M.toList . dvars $ decls
        argsOffs = compOffsets . getParams $ func


genFichier :: TFichier -> Asm ()
genFichier (TFichier _ decl instrs) =
    genFunction [] (TProcedure (TParams [])) decl instrs

-- Stack when a function is called;
-- some space to give the return value
-- argument n
-- ...
-- argument 1
-- %rbp father
-- return address
-- %rbp of caller
-- local variables...
genFunction :: [(TDecls, DeclSizes)] -> Functionnal -> TDecls -> NonEmptyList TInstr -> Asm ()
genFunction prev func decl instrs = do
    pushq rbp
    movq rsp rbp
    subq fs rsp
    mapM_ genInstr instrs
    movq (int 0) rax
    addq fs rsp
    popq rbp
    ret

  where

    fs = frameSize . snd . last $ decls
    decls :: [(TDecls, DeclSizes)]
    decls = prev ++ [(decl, mkDeclSizes prev func decl)]

    typedSize :: Typed -> Integer
    typedSize t = getTypedSize' t id (getSize decls)

    getOffsets :: Map String Typed -> Map String Integer
    getOffsets vars = M.fromList $ zip (map fst varsList) sizes
      where
        varsList = M.toList vars
        sizes = scanl (+) 0 . map (typedSize . snd) $ varsList

    genAccess :: TAccess -> Asm ()
    genAccess (AccessFull id) = do
        --let off = foldr (+) 0 . map (\(CType t _ _) -> typedSize t) .  map (fst . snd) . takeWhile (\(s,_) -> s /= str) $ M.toList (dvars decls)
        let off = getOffset decls id
        leaq (Pointer rbp (-off)) rax

    genAccess (AccessPart e str) = do
        genExpr e
        case snd e of
            CType (TAccess i) _ _ -> error "TODO"
            CType (TRecord (level, name)) _ _ ->
                let (Record rec) = dtypes (fst $ decls !! fromIntegral (level-1)) ! name in
                let ofs = getOffsets rec in
                leaq (Pointer rax (ofs ! str)) rax
            _ -> error "MEH."

    genInstr :: TInstr -> Asm ()

    genInstr (TIAssign acc e) = do
        genExpr e
        pushq rax
        genAccess acc
        popq rbx
        movq rbx (Pointer rax 0)

    genInstr (TIIdent s) = do
        call (Label s)

    --Used to call print_int but it shall be
    --really implemented later
    genInstr (TICall s (Last e)) = do
        genExpr e
        pushq rax
        pushq rax --TODO: rbp father
        call (Label s)
        popq rax
        popq rax

    genInstr (TIReturn e) = error "TODO"

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

    genInstr (TIFor id rev from to instrs) = error "TODO"


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

    genExpr (TECall s exprList, _) = error "Not implemented"

    genExpr (TECharval e, _) = do
        genExpr e
