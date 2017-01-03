module CodeGen (genFichier) where
import Typer
import Asm
import AST
import Data.Char (ord)
import Control.Monad
import Control.Monad.Trans.State.Strict
import           Data.Map      (Map, (!))
import qualified Data.Map      as M
import Data.List (elemIndex)
import Data.Maybe (maybe)
import Debug.Trace

data DeclSizes = DeclSizes
    { tsizes :: Map String Integer
    , voffs  :: Map String Integer
    , frameSize :: Integer
    , argsSize :: Integer
    , fctName :: String
    , nbNestedFor :: Integer
    , pointers :: [String]
    }

getSize :: [(TDecls, DeclSizes)] -> TId -> Integer
-- TODO handle better level-0 declarations
getSize decls (0, name) = if name == "integer" then 8 else 1
getSize decls (level, name) = tsizes (snd $ decls !! fromIntegral (level-1)) ! name

getOffset :: [(TDecls, DeclSizes)] -> TId -> Integer
getOffset decls (level, name) = voffs (snd $ decls !! fromIntegral (level-1)) ! name

isPointer :: [(TDecls, DeclSizes)] -> TId -> Bool
isPointer decls (level, name) = name `elem` (pointers (snd $ decls !! fromIntegral (level-1)))

findFunctionnal :: [(TDecls, DeclSizes)] -> String -> Functionnal
findFunctionnal decls s =
    aux . reverse $ (level0 : (map ((M.map fst3) . dfuns . fst) decls))
  where
    aux (h:t) = maybe (aux t) id (M.lookup s h)
    fst3 (x, _, _) = x
    level0 = M.fromList
              [ ("put", TProcedure $ TParams [("o", CType TCharacter False True)])
              , ("print_int__", TProcedure $ TParams [("i", CType TInteger False True)])
              , ("new_line", TProcedure $ TParams [])
              , ("free__", TProcedure $ TParams [("a", CType TypeNull True True)])
              ]

-- Generic function to avoid copy past
getTypedSize' :: Typed -> (Integer -> a) -> (TId -> a) -> a
getTypedSize' t f g =
    case t of
        TInteger -> f 8
        TCharacter -> f 1
        TBoolean -> f 1
        TRecord i -> g i
        TAccess _ -> f 8
        TypeNull -> f 8

ctypeToTyped :: CType -> Typed
ctypeToTyped (CType t _ _) = t

isOut :: CType -> Bool
isOut (CType _ b _) = b

isRecord :: Typed -> Bool
isRecord (TRecord _) = True
isRecord _ = False

isRecOrAccess :: Typed -> Bool
isRecOrAccess (TAccess _) = True
isRecOrAccess (TRecord _) = True
isRecOrAccess _ = False

getParams :: Functionnal -> [(String, CType)]
getParams (TFunction (TParams p) _) = p
getParams (TProcedure (TParams p)) = p

reverse' :: (Foldable t) => t a -> [a]
reverse' = foldl (flip (:)) []

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

type St = StateT (Map String Integer) (Either ())
mkDeclSizes :: [(TDecls, DeclSizes)] -> Functionnal -> String -> NonEmptyList TInstr -> TDecls -> DeclSizes
mkDeclSizes prev func funcname instrs decls = DeclSizes
    { tsizes = recordedSize
    , voffs = (\(x,_,_) -> x)  offsets
    , frameSize = (\(_,x,_) -> x) offsets + 8*nbNestedFor
    , argsSize = (\(_,_,x) -> x) offsets
    , fctName = funcname
    , nbNestedFor = nbNestedFor
    , pointers = pointers
    }
  where
    addMemo :: String -> Integer -> St ()
    addMemo str size = state $ \map -> ((), M.insert str size map)

    getMemo :: String -> St (Maybe Integer)
    getMemo str = state $ \map -> (M.lookup str map, map)

    getRecordedSize :: TId -> St Integer
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

    getTypedSize :: Typed -> St Integer
    getTypedSize t = getTypedSize' t return getRecordedSize

    recordedSize :: Map String Integer
    recordedSize = fromRight $ execStateT (mapM_ getRecordedSize (zip (repeat (fromIntegral $ length prev)) (M.keys (dtypes decls)))) M.empty

    fromRight (Right x) = x

    typedSize :: Typed -> Integer
    typedSize t = getTypedSize' t id (\(level, name) ->
        if fromIntegral level <= length prev then getSize prev (level, name)
        else recordedSize ! name)

    compOffsets :: Bool -> (CType -> Integer) -> [(String, CType)] -> ([(String, Integer)], Integer)
    compOffsets doTail getSize list = (zip (map fst list) ((if doTail then tail else id) sizes), last sizes)
      where sizes = scanl (+) 0 . map (getSize . snd) $ list

    offsets :: (Map String Integer, Integer, Integer)
    offsets = (M.fromList $ (fst varOffs) ++ (map (\(s, i) -> (s, -i-24)) . fst $ argsOffs), snd varOffs, snd argsOffs)
      where
        varOffs = compOffsets True (typedSize . ctypeToTyped) . map (\(s, (t, _)) -> (s, t)) . M.toList . dvars $ decls
        argsOffs = compOffsets False (\t -> if isOut t then 8 else typedSize . ctypeToTyped $ t) . getParams $ func

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

    pointers = map fst . filter (isOut . snd) . getParams $ func

genFichier :: TFichier -> Asm ()
genFichier (TFichier _ decl instrs) =
    genFunction (M.fromList $ map (\x -> (x,x)) ["main", "print_int__", "put", "new_line", "free__"]) [] "main" (TProcedure (TParams [])) decl instrs

-- Stack when a function is called;
-- some space to give the return value
-- argument n
-- ...
-- argument 1
-- %rbp father
-- return address
-- %rbp of caller
-- local variables...
-- 
-- functions labels are of the form main.fn1.fn2 if fn1 is a subfunction of fn1, which is a toplevel
-- function
genFunction :: Map String String -> [(TDecls, DeclSizes)] -> String -> Functionnal -> TDecls -> NonEmptyList TInstr -> Asm ()
genFunction lbls prev name func decl instrs = do
    pushLabelName name
    label (Label lbl)
    pushq rbp
    movq rsp rbp
    when (fs /= 0) $ subq fs rsp
    forM_ (M.toList $ dvars decl) (\(s, (_, e)) ->
        maybe (return ()) (\e' -> genInstr (TIAssign (AccessFull (fromIntegral $ length decls, s)) e')) e
        )
    mapM_ genInstr instrs
    movq (int 0) rax
    genInstr (TIReturn Nothing)
    mapM_ (\(s, f) -> uncurry3 (genFunction new_lbls decls s) f) (M.toList $ dfuns decl)
    popLabelName

  where
    lbl = lbls ! name
    new_lbls = foldl (\m -> \s -> M.insert s (lbl ++ "." ++ s) m) lbls
             $ map fst $ M.toList $ dfuns decl

    fs = frameSize . snd . last $ decls
    decls :: [(TDecls, DeclSizes)]
    decls = prev ++ [(decl, mkDeclSizes prev func name instrs decl)]

    getFctLevel :: String -> Maybe Int
    getFctLevel = flip elemIndex (map (fctName . snd) decls)

    typedSize :: Typed -> Integer
    typedSize t = getTypedSize' t id (getSize decls)

    ctypeSize :: CType -> Integer
    ctypeSize (CType _ True _) = 8
    ctypeSize (CType t _ _) = typedSize t

    getOffsets :: Map String Typed -> Map String Integer
    getOffsets vars = M.fromList $ zip (map fst varsList) sizes
      where
        varsList = M.toList vars
        sizes = scanl (+) 0 . map (typedSize . snd) $ varsList

    genAccess :: TAccess -> Asm Memory
    genAccess (AccessFull id) =
        if fst id > (fromIntegral $ length decls) then do
            let d = snd . last $ decls
            return (Pointer rbp (-((frameSize d) - 8*(nbNestedFor d) + 8*(fst id - (fromIntegral $ length decls)))))
        else do
            let off = getOffset decls id
            movq rbp rax
            replicateM_ (length decls - (fromIntegral $ fst id)) (movq (Pointer rax 16) rax)
            if isPointer decls id then do
                movq (Pointer rax (-off)) rax
                return (Pointer rax 0)
            else do
                return (Pointer rax (-off))

    genAccess (AccessPart e str) = do
        genExpr e
        case snd e of
            CType (TAccess (level,name)) _ _ ->
                if (str /= "all") then
                    let (Record rec) = dtypes (fst $ decls !! fromIntegral (level-1)) ! name in
                    let ofs = getOffsets rec in
                    return (Pointer rax (ofs ! str))
                else return (Pointer rax 0)
            CType (TRecord (level, name)) _ _ ->
                let (Record rec) = dtypes (fst $ decls !! fromIntegral (level-1)) ! name in
                let ofs = getOffsets rec in
                return (Pointer rax (ofs ! str))
            _ -> error "MEH."

    doFctCall :: String -> [TPExpr] -> Asm ()
    doFctCall s args = do
        let argsIsOut = (reverse' (zip args (map (isOut . snd) . getParams . findFunctionnal decls $ s)))
        forM_  argsIsOut (\(e, out) ->
            if out then
                case fst e of
                    TEAccess a -> do
                        ac <- genAccess a
                        leaq ac rax
                        pushq rax
                    _ -> error "'out' argument is not an access!"
            else do
                let t = ctypeToTyped . snd $ e
                genExpr e
                case t of
                    TInteger -> do
                        pushq rax
                    TCharacter -> do
                        subq (int 1) rsp
                        movb al (Pointer rsp 0)
                    TBoolean -> do
                        subq (int 1) rsp
                        movb al (Pointer rsp 0)
                    TRecord i -> do
                        let size = typedSize t
                        subq size rsp
                        bigCopy (Pointer rax 0) (Pointer rsp 0) size
                    TAccess _ -> do
                        pushq rax
                    TypeNull -> do
                        pushq (int 0)
            )
        if s `elem` ["print_int__", "new_line", "put", "free__"] then
            pushq rbp
        else
            maybe (pushq rbp) (\lev -> do
                movq rbp rax
                replicateM_ (length decls - lev) (movq (Pointer rax 16) rax)
                pushq rax
            ) (getFctLevel s)
        call (Label $ new_lbls ! s)
        popq rax
        let toAdd = sum . map (\(e, out) -> if out then 8 else typedSize . ctypeToTyped . snd $ e) $ argsIsOut
        when (toAdd /= 0) $ addq toAdd rsp

    bigCopy :: Memory -> Memory -> Integer -> Asm ()
    bigCopy m1 m2 size
        | size >= 8 = do
            movq m1 r11
            movq r11 m2
            bigCopy (addOffset 8 m1) (addOffset 8 m2) (size-8)
        | size >= 4 = do
            movl m1 r11d
            movl r11d m2
            bigCopy (addOffset 4 m1) (addOffset 4 m2) (size-4)
        | size >= 2 = do
            movw m1 r11w
            movw r11w m2
            bigCopy (addOffset 2 m1) (addOffset 2 m2) (size-2)
        | size >= 1 = do
            movb m1 r11b
            movb r11b m2
            bigCopy (addOffset 1 m1) (addOffset 1 m2) (size-1)
        | otherwise = do
            return ()

    genInstr :: TInstr -> Asm ()

    genInstr (TIAssign acc e) = do
        genExpr e
        case ctypeToTyped . snd $ e of
            TInteger -> do
                pushq rax
                memAcc <- genAccess acc
                popq rbx
                movq rbx memAcc
            TCharacter -> do
                pushq rax
                memAcc <- genAccess acc
                popq rbx
                movb bl memAcc
            TBoolean -> do
                pushq rax
                memAcc <- genAccess acc
                popq rbx
                movb bl memAcc
            TRecord i -> do
                movq rax r10 -- r10 is not used anywhere else
                memAcc <- genAccess acc
                bigCopy (Pointer r10 0) memAcc (getSize decls i)
            TAccess _ -> do
                pushq rax
                memAcc <- genAccess acc
                popq rbx
                movq rbx memAcc
            TypeNull -> do
                -- Small optimisation : genExpr only wrote 0 to rax
                memAcc <- genAccess acc
                movq (int 0) memAcc

    genInstr (TIIdent s) = do
        doFctCall s []

    genInstr (TICall s args) = do
        doFctCall s (foldr (:) [] args)

    genInstr (TIReturn Nothing) = do
        when (fs /= 0) $ addq fs rsp
        popq rbp
        movq (int 0) rax
        ret

    genInstr (TIReturn (Just e)) = do
        genExpr e
        let off = (+24) . argsSize . snd . last $ decls
        case ctypeToTyped . snd $ e of
            TInteger -> do
                movq rax (Pointer rbp off)
            TCharacter -> do
                movb al (Pointer rbp off)
            TBoolean -> do
                movb al (Pointer rbp off)
            TRecord i -> do
                bigCopy (Pointer rax 0) (Pointer rbp off) (getSize decls i)
            TAccess _ -> do
                movq rax (Pointer rbp off)
            TypeNull -> do
                movq (int 0) (Pointer rbp off)
        genInstr (TIReturn Nothing)

    -- It's useful only for the typer to shadow names
    genInstr (TIBegin le) = do
        mapM_ genInstr le

    genInstr (TIIf lci eci) = do
        pushLabelName "if"
        endLabel <- getLabel "endCond"
        forM_ lci (\(cond, instrs) -> do
            nextLabel <- getLabel "nextCond"
            genExpr cond
            testq rax rax
            jz nextLabel
            mapM_ genInstr instrs
            jmp endLabel
            label nextLabel
            )
        forM_ eci (mapM_ genInstr)
        label endLabel
        popLabelName

    genInstr (TIFor id rev from to instrs) = do
        pushLabelName "for"
        if rev then genExpr to
        else genExpr from
        pushq rax
        memId <- genAccess (AccessFull id)
        popq rbx
        movq rbx memId
        if rev then genExpr from
        else genExpr to
        pushq rax
        bodyLabel <- getLabel "body"
        condLabel <- getLabel "condition"
        jmp condLabel
        label bodyLabel
        mapM_ genInstr instrs
        memId <- genAccess (AccessFull id)
        movq memId rbx
        if rev then decq rbx
        else incq rbx
        movq rbx memId
        label condLabel
        memId <- genAccess (AccessFull id)
        movq memId rax
        popq rbx
        pushq rbx
        cmpq rax rbx
        if rev then jle bodyLabel
        else jge bodyLabel
        popq rbx
        popLabelName


    genInstr (TIWhile cond instrs) = do
        pushLabelName "while"
        bodyLabel <- getLabel "body"
        condLabel <- getLabel "condition"
        jmp condLabel
        label bodyLabel
        mapM_ genInstr instrs
        label condLabel
        genExpr cond
        testq rax rax
        jnz bodyLabel
        popLabelName


    genExpr :: TPExpr -> Asm ()

    genExpr (TEInt i, _) = do
        movq i rax

    genExpr (TEChar c, _) = do
        movq (int . ord $ c) rax

    genExpr (TEBool b, _) = do
        movq (int $ (if b then 1 else 0)) rax

    genExpr (TENull, _) = do
        movq (int 0) rax

    genExpr (TEAccess a, (CType t _ _)) = do
        memAcc <- genAccess a
        case t of
            TInteger -> do
                movq memAcc rax
            TCharacter -> do
                movzbq memAcc rax
            TBoolean -> do
                movzbq memAcc rax
            TRecord i -> do
                leaq memAcc rax
            TAccess _ -> do
                movq memAcc rax
            TypeNull -> do -- Shouldn't happen
                movq (int 0) rax


    genExpr (TEBinop op e1 e2, ct) =
        case op of
            Equal -> case ctypeToTyped . snd $ e1 of
                TRecord i -> do
                    evalBothExpr
                    falseLab <- getLabel "receqFalse"
                    endLab <- getLabel "receqEnd"
                    bigCond rax rbx (getSize decls i) 0 falseLab
                    movq (int 1) rax
                    jmp endLab
                    label falseLab
                    movq (int 0) rax
                    label endLab
                _ -> evalCond sete
            NotEqual -> do
                genExpr (TEBinop Equal e1 e2, ct)
                xorq (int 1) rax
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
            AndThen -> do
                genExpr e1
                testb al al
                lbl <- getLabel "andthen"
                je lbl
                genExpr e2
                label lbl
            Or -> do
                evalBothExpr
                orq rbx rax
            OrElse -> do
                genExpr e1
                testb al al
                lbl <- getLabel "orelse"
                jne lbl
                genExpr e2
                label lbl
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
        bigCond :: Register -> Register -> Integer -> Integer -> Label -> Asm ()
        bigCond reg1 reg2 size off falseLab
            | size >= 8 = do
                movq (Pointer reg1 off) r11
                cmpq r11 (Pointer reg2 off)
                jnz falseLab
                bigCond reg1 reg2 (size-8) (off+8) falseLab
            | size >= 4 = do
                movl (Pointer reg1 off) r11d
                cmpl r11d (Pointer reg2 off)
                jnz falseLab
                bigCond reg1 reg2 (size-4) (off+4) falseLab
            | size >= 2 = do
                movw (Pointer reg1 off) r11w
                cmpw r11w (Pointer reg2 off)
                jnz falseLab
                bigCond reg1 reg2 (size-2) (off+2) falseLab
            | size >= 1 = do
                movb (Pointer reg1 off) r11b
                cmpb r11b (Pointer reg2 off)
                jnz falseLab
                bigCond reg1 reg2 (size-1) (off+1) falseLab
            | otherwise = do
                return ()

    genExpr (TEUnop op e, _) = do
        genExpr e
        case op of
            Not -> do
                xorq (int 1) rax
            Negate -> do
                negq rax

    genExpr (TENew s, _) = do
        let size = getSize decls s
        movq size rdi
        call (Label "malloc")

    genExpr (TECall s args, CType t _ _) = do
        subq (typedSize t) rsp
        doFctCall s args
        case t of
            TInteger ->
                movq (Pointer rsp 0) rax
            TCharacter ->
                movzbq (Pointer rsp 0) rax
            TBoolean ->
                movzbq (Pointer rsp 0) rax
            TRecord i ->
                movq rsp rax
            TAccess _ ->
                movq (Pointer rsp 0) rax
            TypeNull -> -- Shouldn't happen
                movq (int 0) rax
        addq (typedSize t) rsp



    genExpr (TECharval e, _) = do
        genExpr e
