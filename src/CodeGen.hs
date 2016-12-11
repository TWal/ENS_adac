module CodeGen (genFichier) where
import Typer
import Asm
import AST
import Data.Char (ord)
import Control.Monad
import Control.Monad.State
import           Data.Map      (Map, (!))
import qualified Data.Map      as M

genFichier :: TFichier -> Asm ()
genFichier (TFichier _ decls l) = do
    pushq rbp
    movq rsp rbp
    subq (int 1000) rsp
    mapM_ genInstr l
    movq (int 0) rax
    addq (int 1000) rsp
    popq rbp
    ret

  where

    addMemo :: String -> Integer -> State (Map String Integer) ()
    addMemo str size = state $ \map -> ((), M.insert str size map)

    getMemo :: String -> State (Map String Integer) (Maybe Integer)
    getMemo str = state $ \map -> (M.lookup str map, map)

    getRecordedSize :: String -> State (Map String Integer) Integer
    getRecordedSize name = do
        tryMemo <- getMemo name
        case tryMemo of
            Nothing -> do
                size <- case (dtypes decls) ! name of
                    Record m ->
                        foldM (\acc ty -> do
                            curSize <- getTypedSize ty
                            return $ acc + curSize
                        ) 0 m
                    RNotDefined -> error "MEH"
                    RAlias i -> getRecordedSize . snd $ i
                    RNType alias -> getTypedSize alias
                addMemo name size
                return size
            Just size -> return size

    getTypedSize :: Typed -> State (Map String Integer) Integer
    getTypedSize t =
        case t of
            TInteger -> return 8
            TCharacter -> return 1
            TBoolean -> return 1
            TRecord i -> getRecordedSize . snd $ i
            TAccess _ -> return 8
            TypeNull -> error "ME DUNNO WAT IZ TEH SIZE OF NULL!!1!"

    recordedSize :: Map String Integer
    recordedSize = execState (mapM_ getRecordedSize (M.keys (dtypes decls))) M.empty

    --TODO: remove copy paste
    typedSize :: Typed -> Integer
    typedSize t =
        case t of
            TInteger -> 8
            TCharacter -> 1
            TBoolean -> 1
            TRecord i -> recordedSize ! (snd i)
            TAccess _ -> 8
            TypeNull -> error "ME DUNNO WAT IZ TEH SIZE OF NULL!!1!"

    computeAccessAddr :: TAccess -> Asm ()
    computeAccessAddr (AccessFull str) = do
        let off = foldr (+) 0 . map (\(CType t _ _) -> typedSize t) .  map (fst . snd) . takeWhile (\(s,_) -> s /= str) $ M.toList (dvars decls)
        leaq (Pointer rbp (off+42)) rax
    computeAccessAddr (AccessPart e str) = error "Not implemented"


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
