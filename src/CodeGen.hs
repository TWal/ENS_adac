module CodeGen (genFichier) where
import Typer
import Asm
import AST
import Data.Char (ord)
import Control.Monad

genFichier :: TFichier -> Asm ()
genFichier (TFichier _ _ l) = do
    mapM_ genInstr l
    movq (int 0) rax
    ret

genInstr :: TInstr -> Asm ()

genInstr (TIAssign acc e) = error "TODO"

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

genInstr (TIFor id rev from to linstr) = error "TODO"

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
genAccess _ = error "genAccess not fully implemented!"
