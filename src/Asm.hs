module Asm (
    Register, Label, Memory(..), Star, RValue(..), LValue(..), Jmpable(..),
    getLabel, getAssembly, star,
    rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp, r8, r9, r10, r11, r12, r13, r14, r15,
    eax, ebx, ecx, edx, esi, edi, ebp, esp, r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d,
    ax , bx , cx , dx , si , di , bp , sp , r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w,
    al , bl , cl , dl , ah , bh , ch , dh , sil, dil, bpl , spl , r8b , r9b , r10b, r11b, r12b, r13b, r14b, r15b,
    movb, movw, movl, movq, movabsq, movsbw, movsbl, movsbq, movswl, movswq, movslq, movzbw, movzbl, movzbq, movzwl, movzwq, pushq, popq,
    leab, leaw, leal, leaq, incb, incw, incl, incq, decb, decw, decl, decq, negb, negw, negl, negq, notb, notw, notl, notq,
    addb, addw, addl, addq, subb, subw, subl, subq, imulw, imull, imulq,
    xorb, xorw, xorl, xorq, orb, orw, orl, orq, andb, andw, andl, andq,
    idivl, divl, cltd, idivq, divq, cqto,
    sarb, sarw, sarl, sarq, shlb, shlw, shll, shlq, shrb, shrw, shrl, shrq,
    cmpb, cmpw, cmpl, cmpq, testb, testw, testl, testq,
    je, jne, js, jns, jg, jge, jl, jle, ja, jae, jb, jbe,
    sete, setne, sets, setns, setg, setge, setl, setle, seta, setae, setb, setbe,
    cmove, cmovne, cmovs, cmovns, cmovg, cmovge, cmovl, cmovle, cmova, cmovae, cmovb, cmovbe,
    jmp, call, leave, ret
) where

import Data.Char (toLower)
import Control.Monad.State

data Register =
    Rax | Rbx | Rcx | Rdx | Rsi | Rdi | Rbp | Rsp | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  | Eax | Ebx | Ecx | Edx | Esi | Edi | Ebp | Esp | R8d | R9d | R10d | R11d | R12d | R13d | R14d | R15d
  | Ax  | Bx  | Cx  | Dx  | Si  | Di  | Bp  | Sp  | R8w | R9w | R10w | R11w | R12w | R13w | R14w | R15w
  | Al  | Bl  | Cl  | Dl  | Ah  | Bh  | Ch  | Dh  | Sil | Dil | Bpl  | Spl  | R8b  | R9b  | R10b | R11b | R12b | R13b | R14b | R15b
    deriving (Show)

data Label = Label String

data Memory = Memory Register Integer Integer Integer

data Star a = Star a

class RValue a where
    arg :: a -> String

instance RValue Integer where
    arg i = "$" ++ show i

instance RValue Label where
    arg (Label s) = s

instance RValue Register where
    arg r = case (show r) of
              x:xs -> '%':(toLower x):xs
              [] -> "%" -- shouldn't happen

instance RValue Memory where
    arg (Memory base index scale offset) = (show offset) ++ "(" ++ (arg base) ++ ", " ++ (show index) ++ ", " ++ (show scale) ++ ")"

class (RValue a) => LValue a
instance LValue Register
instance LValue Memory

class Jmpable a where
    labToStr :: a -> String

instance Jmpable Label where
    labToStr (Label s) = s

instance (RValue a) => Jmpable (Star a) where
    labToStr (Star x) = "*(" ++ arg x ++ ")"

type Asm = State (String, Integer)

addCode :: String -> Asm ()
addCode s = state $ \(str, i) -> ((), (str ++ s ++ "\n", i))

getLabel :: Asm Label
getLabel = state $ \(str, i) -> (Label $ "label" ++ show i, (str, i+1))

getAssembly :: Asm a -> String
getAssembly = fst . flip execState ("", 0)

-- Helper functions
ins0 :: String -> Asm ()
ins0 s = addCode s

ins1 :: RValue a => String -> a -> Asm ()
ins1 s r1 = addCode $ s ++ " " ++ arg r1

ins2 :: (RValue a, RValue b) => String -> a -> b -> Asm ()
ins2 s r1 r2 = addCode $ s ++ " " ++ arg r1 ++ ", " ++ arg r2

genericJump :: (Jmpable a) => String -> a -> Asm ()
genericJump s l = addCode $ s ++ " " ++ labToStr l

-- To make jmp *(...)
star :: (RValue a) => a -> Star a
star = Star

-- Nicer names for registers
rax = Rax
rbx = Rbx
rcx = Rcx
rdx = Rdx
rsi = Rsi
rdi = Rdi
rbp = Rbp
rsp = Rsp
r8 = R8
r9 = R9
r10 = R10
r11 = R11
r12 = R12
r13 = R13
r14 = R14
r15 = R15
eax = Eax
ebx = Ebx
ecx = Ecx
edx = Edx
esi = Esi
edi = Edi
ebp = Ebp
esp = Esp
r8d = R8d
r9d = R9d
r10d = R10d
r11d = R11d
r12d = R12d
r13d = R13d
r14d = R14d
r15d = R15d
ax = Ax
bx = Bx
cx = Cx
dx = Dx
si = Si
di = Di
bp = Bp
sp = Sp
r8w = R8w
r9w = R9w
r10w = R10w
r11w = R11w
r12w = R12w
r13w = R13w
r14w = R14w
r15w = R15w
al = Al
bl = Bl
cl = Cl
dl = Dl
ah = Ah
bh = Bh
ch = Ch
dh = Dh
sil = Sil
dil = Dil
bpl = Bpl
spl = Spl
r8b = R8b
r9b = R9b
r10b = R10b
r11b = R11b
r12b = R12b
r13b = R13b
r14b = R14b
r15b = R15b


-- Instructions
label :: Label -> Asm ()
label (Label s) = addCode $ s ++ ":"

movb :: (RValue a, LValue b) => a -> b -> Asm ()
movw :: (RValue a, LValue b) => a -> b -> Asm ()
movl :: (RValue a, LValue b) => a -> b -> Asm ()
movq :: (RValue a, LValue b) => a -> b -> Asm ()

movb = ins2 "movb"
movw = ins2 "movw"
movl = ins2 "movl"
movq = ins2 "movq"

movabsq :: (LValue b) => Integer -> b -> Asm ()
movsbw :: (RValue a, LValue b) => a -> b -> Asm ()
movsbl :: (RValue a, LValue b) => a -> b -> Asm ()
movsbq :: (RValue a, LValue b) => a -> b -> Asm ()
movswl :: (RValue a, LValue b) => a -> b -> Asm ()
movswq :: (RValue a, LValue b) => a -> b -> Asm ()
movslq :: (RValue a, LValue b) => a -> b -> Asm ()
movzbw :: (RValue a, LValue b) => a -> b -> Asm ()
movzbl :: (RValue a, LValue b) => a -> b -> Asm ()
movzbq :: (RValue a, LValue b) => a -> b -> Asm ()
movzwl :: (RValue a, LValue b) => a -> b -> Asm ()
movzwq :: (RValue a, LValue b) => a -> b -> Asm ()
pushq :: (RValue a) => a -> Asm ()
popq :: (LValue a) => a -> Asm ()
movabsq = ins2 "movabsq"
movsbw = ins2 "movsbw"
movsbl = ins2 "movsbl"
movsbq = ins2 "movsbq"
movswl = ins2 "movswl"
movswq = ins2 "movswq"
movslq = ins2 "movslq"
movzbw = ins2 "movzbw"
movzbl = ins2 "movzbl"
movzbq = ins2 "movzbq"
movzwl = ins2 "movzwl"
movzwq = ins2 "movzwq"
pushq = ins1 "pushq"
popq = ins1 "popq"

leab :: Memory -> Register -> Asm ()
leaw :: Memory -> Register -> Asm ()
leal :: Memory -> Register -> Asm ()
leaq :: Memory -> Register -> Asm ()
incb :: (LValue a) => a -> Asm ()
incw :: (LValue a) => a -> Asm ()
incl :: (LValue a) => a -> Asm ()
incq :: (LValue a) => a -> Asm ()
decb :: (LValue a) => a -> Asm ()
decw :: (LValue a) => a -> Asm ()
decl :: (LValue a) => a -> Asm ()
decq :: (LValue a) => a -> Asm ()
negb :: (LValue a) => a -> Asm ()
negw :: (LValue a) => a -> Asm ()
negl :: (LValue a) => a -> Asm ()
negq :: (LValue a) => a -> Asm ()
notb :: (RValue a) => a -> Asm ()
notw :: (RValue a) => a -> Asm ()
notl :: (RValue a) => a -> Asm ()
notq :: (RValue a) => a -> Asm ()
leab = ins2 "leab"
leaw = ins2 "leaw"
leal = ins2 "leal"
leaq = ins2 "leaq"
incb = ins1 "incb"
incw = ins1 "incw"
incl = ins1 "incl"
incq = ins1 "incq"
decb = ins1 "decb"
decw = ins1 "decw"
decl = ins1 "decl"
decq = ins1 "decq"
negb = ins1 "negb"
negw = ins1 "negw"
negl = ins1 "negl"
negq = ins1 "negq"
notb = ins1 "notb"
notw = ins1 "notw"
notl = ins1 "notl"
notq = ins1 "notq"

addb :: (RValue a, LValue b) => a -> b -> Asm ()
addw :: (RValue a, LValue b) => a -> b -> Asm ()
addl :: (RValue a, LValue b) => a -> b -> Asm ()
addq :: (RValue a, LValue b) => a -> b -> Asm ()
subb :: (RValue a, LValue b) => a -> b -> Asm ()
subw :: (RValue a, LValue b) => a -> b -> Asm ()
subl :: (RValue a, LValue b) => a -> b -> Asm ()
subq :: (RValue a, LValue b) => a -> b -> Asm ()
imulw :: (RValue a) => a -> Register -> Asm ()
imull :: (RValue a) => a -> Register -> Asm ()
imulq :: (RValue a) => a -> Register -> Asm ()
addb = ins2 "addb"
addw = ins2 "addw"
addl = ins2 "addl"
addq = ins2 "addq"
subb = ins2 "subb"
subw = ins2 "subw"
subl = ins2 "subl"
subq = ins2 "subq"
imulw = ins2 "imulw"
imull = ins2 "imull"
imulq = ins2 "imulq"

xorb :: (RValue a, LValue b) => a -> b -> Asm ()
xorw :: (RValue a, LValue b) => a -> b -> Asm ()
xorl :: (RValue a, LValue b) => a -> b -> Asm ()
xorq :: (RValue a, LValue b) => a -> b -> Asm ()
orb :: (RValue a, LValue b) => a -> b -> Asm ()
orw :: (RValue a, LValue b) => a -> b -> Asm ()
orl :: (RValue a, LValue b) => a -> b -> Asm ()
orq :: (RValue a, LValue b) => a -> b -> Asm ()
andb :: (RValue a, LValue b) => a -> b -> Asm ()
andw :: (RValue a, LValue b) => a -> b -> Asm ()
andl :: (RValue a, LValue b) => a -> b -> Asm ()
andq :: (RValue a, LValue b) => a -> b -> Asm ()
xorb = ins2 "xorb"
xorw = ins2 "xorw"
xorl = ins2 "xorl"
xorq = ins2 "xorq"
orb = ins2 "orb"
orw = ins2 "orw"
orl = ins2 "orl"
orq = ins2 "orq"
andb = ins2 "andb"
andw = ins2 "andw"
andl = ins2 "andl"
andq = ins2 "andq"


idivl :: (RValue a) => a -> Asm ()
divl :: (RValue a) => a -> Asm ()
cltd :: Asm ()
idivq :: (RValue a) => a -> Asm ()
divq :: (RValue a) => a -> Asm ()
cqto :: Asm ()
idivl = ins1 "idivl"
divl = ins1 "divl"
cltd = ins0 "cltd"
idivq = ins1 "idivq"
divq = ins1 "divq"
cqto = ins0 "cqto"

-- The type of these instructions are quite complicated in reality.
-- sarl seems to work when the first argument is %cl but not when it is %al...
sarb :: (RValue a, LValue b) => a -> b -> Asm ()
sarw :: (RValue a, LValue b) => a -> b -> Asm ()
sarl :: (RValue a, LValue b) => a -> b -> Asm ()
sarq :: (RValue a, LValue b) => a -> b -> Asm ()
shlb :: (RValue a, LValue b) => a -> b -> Asm ()
shlw :: (RValue a, LValue b) => a -> b -> Asm ()
shll :: (RValue a, LValue b) => a -> b -> Asm ()
shlq :: (RValue a, LValue b) => a -> b -> Asm ()
shrb :: (RValue a, LValue b) => a -> b -> Asm ()
shrw :: (RValue a, LValue b) => a -> b -> Asm ()
shrl :: (RValue a, LValue b) => a -> b -> Asm ()
shrq :: (RValue a, LValue b) => a -> b -> Asm ()
sarb = ins2 "sarb"
sarw = ins2 "sarw"
sarl = ins2 "sarl"
sarq = ins2 "sarq"
shlb = ins2 "shlb"
shlw = ins2 "shlw"
shll = ins2 "shll"
shlq = ins2 "shlq"
shrb = ins2 "shrb"
shrw = ins2 "shrw"
shrl = ins2 "shrl"
shrq = ins2 "shrq"

cmpb :: (RValue a, RValue b) => a -> b -> Asm ()
cmpw :: (RValue a, RValue b) => a -> b -> Asm ()
cmpl :: (RValue a, RValue b) => a -> b -> Asm ()
cmpq :: (RValue a, RValue b) => a -> b -> Asm ()
testb :: (RValue a, RValue b) => a -> b -> Asm ()
testw :: (RValue a, RValue b) => a -> b -> Asm ()
testl :: (RValue a, RValue b) => a -> b -> Asm ()
testq :: (RValue a, RValue b) => a -> b -> Asm ()
cmpb = ins2 "cmpb"
cmpw = ins2 "cmpw"
cmpl = ins2 "cmpl"
cmpq = ins2 "cmpq"
testb = ins2 "testb"
testw = ins2 "testw"
testl = ins2 "testl"
testq = ins2 "testq"

je  :: (Jmpable a) => a -> Asm ()
jne :: (Jmpable a) => a -> Asm ()
js  :: (Jmpable a) => a -> Asm ()
jns :: (Jmpable a) => a -> Asm ()
jg  :: (Jmpable a) => a -> Asm ()
jge :: (Jmpable a) => a -> Asm ()
jl  :: (Jmpable a) => a -> Asm ()
jle :: (Jmpable a) => a -> Asm ()
ja  :: (Jmpable a) => a -> Asm ()
jae :: (Jmpable a) => a -> Asm ()
jb  :: (Jmpable a) => a -> Asm ()
jbe :: (Jmpable a) => a -> Asm ()
je  = genericJump "je"
jne = genericJump "jne"
js  = genericJump "js"
jns = genericJump "jns"
jg  = genericJump "jg"
jge = genericJump "jge"
jl  = genericJump "jl"
jle = genericJump "jle"
ja  = genericJump "ja"
jae = genericJump "jae"
jb  = genericJump "jb"
jbe = genericJump "jbe"

sete  :: (LValue a) => a -> Asm ()
setne :: (LValue a) => a -> Asm ()
sets  :: (LValue a) => a -> Asm ()
setns :: (LValue a) => a -> Asm ()
setg  :: (LValue a) => a -> Asm ()
setge :: (LValue a) => a -> Asm ()
setl  :: (LValue a) => a -> Asm ()
setle :: (LValue a) => a -> Asm ()
seta  :: (LValue a) => a -> Asm ()
setae :: (LValue a) => a -> Asm ()
setb  :: (LValue a) => a -> Asm ()
setbe :: (LValue a) => a -> Asm ()
sete  = ins1 "sete"
setne = ins1 "setne"
sets  = ins1 "sets"
setns = ins1 "setns"
setg  = ins1 "setg"
setge = ins1 "setge"
setl  = ins1 "setl"
setle = ins1 "setle"
seta  = ins1 "seta"
setae = ins1 "setae"
setb  = ins1 "setb"
setbe = ins1 "setbe"

cmove  :: (RValue a, LValue b) => a -> b -> Asm ()
cmovne :: (RValue a, LValue b) => a -> b -> Asm ()
cmovs  :: (RValue a, LValue b) => a -> b -> Asm ()
cmovns :: (RValue a, LValue b) => a -> b -> Asm ()
cmovg  :: (RValue a, LValue b) => a -> b -> Asm ()
cmovge :: (RValue a, LValue b) => a -> b -> Asm ()
cmovl  :: (RValue a, LValue b) => a -> b -> Asm ()
cmovle :: (RValue a, LValue b) => a -> b -> Asm ()
cmova  :: (RValue a, LValue b) => a -> b -> Asm ()
cmovae :: (RValue a, LValue b) => a -> b -> Asm ()
cmovb  :: (RValue a, LValue b) => a -> b -> Asm ()
cmovbe :: (RValue a, LValue b) => a -> b -> Asm ()
cmove  = ins2 "cmove"
cmovne = ins2 "cmovne"
cmovs  = ins2 "cmovs"
cmovns = ins2 "cmovns"
cmovg  = ins2 "cmovg"
cmovge = ins2 "cmovge"
cmovl  = ins2 "cmovl"
cmovle = ins2 "cmovle"
cmova  = ins2 "cmova"
cmovae = ins2 "cmovae"
cmovb  = ins2 "cmovb"
cmovbe = ins2 "cmovbe"

jmp :: (Jmpable a) => a -> Asm ()
call :: (Jmpable a) => a -> Asm ()
jmp = genericJump "jmp"
call = genericJump "call"

leave :: Asm ()
ret :: Asm ()
leave = ins0 "leave"
ret = ins0 "ret"

dumbAssembly = do
    movq rax rbx
    l <- getLabel
    testq rax rax
    je (star rax)
    l' <- getLabel
    jne l'
    label l
    addq rax rbx
    ret
    label l'
    subq rax rbx
    ret
