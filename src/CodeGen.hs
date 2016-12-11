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
    }

getSize :: [(TDecls, DeclSizes)] -> TId -> Integer
getSize decls (level, name) = tsizes (snd $ decls !! fromIntegral level) ! name

getOffset :: [(TDecls, DeclSizes)] -> TId -> Integer
getOffset decls (level, name) = voffs (snd $ decls !! fromIntegral level) ! name

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

mkDeclSizes :: [(TDecls, DeclSizes)] -> TDecls -> DeclSizes
mkDeclSizes prev decls = DeclSizes
    { tsizes = recordedSize
    , voffs = fst varOffsets
    , frameSize = snd varOffsets
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

    varOffsets :: (Map String Integer, Integer)
    varOffsets = (M.fromList $ zip (map fst varsList) (tail sizes), last sizes)
      where
        varsList = M.toList (dvars decls)
        sizes :: [Integer]
        sizes = scanl (+) 0 . map ((\(CType t _ _) -> typedSize t) . fst . snd) $ varsList


genFichier :: TFichier -> Asm ()
genFichier (TFichier _ decl instrs) =
    genFunction [] decl instrs

genFunction :: [(TDecls, DeclSizes)] -> TDecls -> NonEmptyList TInstr -> Asm ()
genFunction prev decl instrs = do
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
    decls = prev ++ [(decl, mkDeclSizes prev decl)]

    typedSize :: Typed -> Integer
    typedSize t = getTypedSize' t id (getSize prev)

    getOffsets :: Bool -> Map String Typed -> Map String Integer
    getOffsets isStack vars = M.fromList $ zip (map fst varsList) sizes
      where
        varsList = M.toList vars
        sizes = tail . scanl isStack (+) 0 . map ((\(CType t _ _) -> typedSize t) . fst . snd) $ varsList

    computeAccessAddr :: TAccess -> Asm ()
    computeAccessAddr (AccessFull str) = do
        --let off = foldr (+) 0 . map (\(CType t _ _) -> typedSize t) .  map (fst . snd) . takeWhile (\(s,_) -> s /= str) $ M.toList (dvars decls)
        --TODO: get in the last "env", but AccessFull should contain a TId, not a String
        let off = getOffset decls (fromIntegral $ length decls - 1, str)
        leaq (Pointer rbp (-off)) rax

    computeAccessAddr (AccessPart e str) = do
        genExpr $ snd e
        case fst e of
            TAccess i -> error "TODO"
            TRecord i -> error "TODO"
            _ -> error "MEH."

    genInstr :: TInstr -> Asm ()

    genInstr (TIAssign acc e) = do
        genExpr $ fst e
        pushq rax
        computeAccessAddr acc
        popq rbx
        movq rbx (Pointer rax 0)

    genInstr (TIIdent s) = error "TODO"

    --Used to call print_int but it shall be
    --really implemented later
    genInstr (TICall s (Last e)) = do
        genExpr . fst $ e
        movq rax rdi
        call (Label s)

    genInstr (TIReturn e) = error "TODO"

    -- It's useful only for the typer to shadow names
    genInstr (TIBegin le) = do
        mapM_ genInstr le

    genInstr (TIIf lci eci) = do
        endLabel <- getLabel
        forM_ lci (\(cond, instrs) -> do
            nextLabel <- getLabel
            genExpr . fst $ cond
            testq rax rax
            jz nextLabel
            mapM_ genInstr instrs
            jmp endLabel
            label nextLabel
            )
        forM_ eci (mapM_ genInstr)
        label endLabel

    genInstr (TIFor id rev from to instr) = error "TODO"

    genInstr (TIWhile cond instrs) = do
        bodyLabel <- getLabel
        condLabel <- getLabel
        jmp condLabel
        label bodyLabel
        mapM_ genInstr instrs
        label condLabel
        genExpr . fst $ cond
        testq rax rax
        jnz bodyLabel


    genExpr :: TExpr -> Asm ()

    genExpr (TEInt i) = do
        movq i rax

    genExpr (TEChar c) = do
        movq (int . ord $ c) rax

    genExpr (TEBool b) = do
        movq (int $ (if b then 1 else 0)) rax

    genExpr TENull = do
        movq (int 0) rax

    genExpr (TEAccess a) = do
        genAccess a

    genExpr (TEBinop op e1 e2) =
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
            genExpr . fst $ e2
            pushq rax
            genExpr . fst $ e1
            popq rbx
        evalCond :: (Register -> Asm ()) -> Asm ()
        evalCond setcond = do
            evalBothExpr
            cmpq rbx rax
            setcond al
            andq (int 1) rax

    genExpr (TEUnop op e) = do
        genExpr .fst $ e
        case op of
            Not -> do
                xorq (int 1) rax
            Negate -> do
                negq rax

    genExpr (TENew s) = error "Not implemented"

    genExpr (TECall s exprList) = error "Not implemented"

    genExpr (TECharval e) = do
        genExpr . fst $ e

    genAccess :: TAccess -> Asm ()
    genAccess ac = do
        computeAccessAddr ac
        movq (Pointer rax 0) rax
