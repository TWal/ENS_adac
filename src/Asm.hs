module Asm where
import Data.Char (toLower)
import Control.Monad.State

data Register =
    Rax | Rbx | Rcx | Rdx | Rsi | Rdi | Rbp | Rsp | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  | Eax | Ebx | Ecx | Edx | Esi | Edi | Ebp | Esp | R8d | R9d | R10d | R11d | R12d | R13d | R14d | R15d
  | Ax  | Bx  | Cx  | Dx  | Si  | Di  | Bp  | Sp  | R8w | R9w | R10w | R11w | R12w | R13w | R14w | R15w
  | Al  | Bl  | Cl  | Dl  | Ah  | Bh  | Ch  | Dh  | Sil | Dil | Bpl  | Spl  | R8b  | R9b  | R10b | R11b | R12b | R13b | R14b | R15b
    deriving (Show)

data Label = Label String

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


class Argument a where
    arg :: a -> String

instance Argument Integer where
    arg i = "$" ++ show i

instance Argument Label where
    arg (Label s) = s

instance Argument Register where
    arg r = case (show r) of
              x:xs -> '%':(toLower x):xs
              [] -> "%" -- shouldn't happen

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

ins1 :: Argument a => String -> a -> Asm ()
ins1 s r1 = addCode $ s ++ " " ++ arg r1

ins2 :: (Argument a, Argument b) => String -> a -> b -> Asm ()
ins2 s r1 r2 = addCode $ s ++ " " ++ arg r1 ++ " " ++ arg r2

-- To make jmp *label
star :: Label -> Label
star (Label s) = Label ('*':s)

-- Instructions
label :: Label -> Asm ()
label (Label s) = addCode $ s ++ ":"

movb :: (Argument a, Argument b) => a -> b -> Asm ()
movw :: (Argument a, Argument b) => a -> b -> Asm ()
movl :: (Argument a, Argument b) => a -> b -> Asm ()
movq :: (Argument a, Argument b) => a -> b -> Asm ()
movb = ins2 "movb"
movw = ins2 "movw"
movl = ins2 "movl"
movq = ins2 "movq"

-- TODO: the first or second argument should be an integer
movabsq :: (Argument a, Argument b) => a -> b -> Asm ()
movsbw :: (Argument a, Argument b) => a -> b -> Asm ()
movsbl :: (Argument a, Argument b) => a -> b -> Asm ()
movsbq :: (Argument a, Argument b) => a -> b -> Asm ()
movswl :: (Argument a, Argument b) => a -> b -> Asm ()
movswq :: (Argument a, Argument b) => a -> b -> Asm ()
movslq :: (Argument a, Argument b) => a -> b -> Asm ()
movzbw :: (Argument a, Argument b) => a -> b -> Asm ()
movzbl :: (Argument a, Argument b) => a -> b -> Asm ()
movzbq :: (Argument a, Argument b) => a -> b -> Asm ()
movzwl :: (Argument a, Argument b) => a -> b -> Asm ()
movzwq :: (Argument a, Argument b) => a -> b -> Asm ()
leab :: (Argument a, Argument b) => a -> b -> Asm ()
leaw :: (Argument a, Argument b) => a -> b -> Asm ()
leal :: (Argument a, Argument b) => a -> b -> Asm ()
leaq :: (Argument a, Argument b) => a -> b -> Asm ()
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
leab = ins2 "leab"
leaw = ins2 "leaw"
leal = ins2 "leal"
leaq = ins2 "leaq"

incb :: (Argument a) => a -> Asm ()
incw :: (Argument a) => a -> Asm ()
incl :: (Argument a) => a -> Asm ()
incq :: (Argument a) => a -> Asm ()
decb :: (Argument a) => a -> Asm ()
decw :: (Argument a) => a -> Asm ()
decl :: (Argument a) => a -> Asm ()
decq :: (Argument a) => a -> Asm ()
negb :: (Argument a) => a -> Asm ()
negw :: (Argument a) => a -> Asm ()
negl :: (Argument a) => a -> Asm ()
negq :: (Argument a) => a -> Asm ()
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

addb :: (Argument a, Argument b) => a -> b -> Asm ()
addw :: (Argument a, Argument b) => a -> b -> Asm ()
addl :: (Argument a, Argument b) => a -> b -> Asm ()
addq :: (Argument a, Argument b) => a -> b -> Asm ()
subb :: (Argument a, Argument b) => a -> b -> Asm ()
subw :: (Argument a, Argument b) => a -> b -> Asm ()
subl :: (Argument a, Argument b) => a -> b -> Asm ()
subq :: (Argument a, Argument b) => a -> b -> Asm ()
imulw :: (Argument a, Argument b) => a -> b -> Asm ()
imull :: (Argument a, Argument b) => a -> b -> Asm ()
imulq :: (Argument a, Argument b) => a -> b -> Asm ()

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

idivq :: (Argument a) => a -> Asm ()
cqto :: Asm ()
idivq = ins1 "idivq"
cqto = ins0 "cqto"

notb :: (Argument a) => a -> Asm ()
notw :: (Argument a) => a -> Asm ()
notl :: (Argument a) => a -> Asm ()
notq :: (Argument a) => a -> Asm ()

notb = ins1 "notb"
notw = ins1 "notw"
notl = ins1 "notl"
notq = ins1 "notq"


andb :: (Argument a, Argument b) => a -> b -> Asm ()
andw :: (Argument a, Argument b) => a -> b -> Asm ()
andl :: (Argument a, Argument b) => a -> b -> Asm ()
andq :: (Argument a, Argument b) => a -> b -> Asm ()
orb :: (Argument a, Argument b) => a -> b -> Asm ()
orw :: (Argument a, Argument b) => a -> b -> Asm ()
orl :: (Argument a, Argument b) => a -> b -> Asm ()
orq :: (Argument a, Argument b) => a -> b -> Asm ()
xorb :: (Argument a, Argument b) => a -> b -> Asm ()
xorw :: (Argument a, Argument b) => a -> b -> Asm ()
xorl :: (Argument a, Argument b) => a -> b -> Asm ()
xorq :: (Argument a, Argument b) => a -> b -> Asm ()
shlb :: (Argument a, Argument b) => a -> b -> Asm ()
shlw :: (Argument a, Argument b) => a -> b -> Asm ()
shll :: (Argument a, Argument b) => a -> b -> Asm ()
shlq :: (Argument a, Argument b) => a -> b -> Asm ()
shrb :: (Argument a, Argument b) => a -> b -> Asm ()
shrw :: (Argument a, Argument b) => a -> b -> Asm ()
shrl :: (Argument a, Argument b) => a -> b -> Asm ()
shrq :: (Argument a, Argument b) => a -> b -> Asm ()
sarb :: (Argument a, Argument b) => a -> b -> Asm ()
sarw :: (Argument a, Argument b) => a -> b -> Asm ()
sarl :: (Argument a, Argument b) => a -> b -> Asm ()
sarq :: (Argument a, Argument b) => a -> b -> Asm ()

andb = ins2 "andb"
andw = ins2 "andw"
andl = ins2 "andl"
andq = ins2 "andq"
orb = ins2 "orb"
orw = ins2 "orw"
orl = ins2 "orl"
orq = ins2 "orq"
xorb = ins2 "xorb"
xorw = ins2 "xorw"
xorl = ins2 "xorl"
xorq = ins2 "xorq"
shlb = ins2 "shlb"
shlw = ins2 "shlw"
shll = ins2 "shll"
shlq = ins2 "shlq"
shrb = ins2 "shrb"
shrw = ins2 "shrw"
shrl = ins2 "shrl"
shrq = ins2 "shrq"
sarb = ins2 "sarb"
sarw = ins2 "sarw"
sarl = ins2 "sarl"
sarq = ins2 "sarq"

jmp :: (Argument a) => a -> Asm ()
call :: (Argument a) => a -> Asm ()
jmp = ins1 "jmp"
call = ins1 "jmp"

leave :: Asm ()
ret :: Asm ()
leave = ins0 "leave"
ret = ins0 "ret"

je :: (Argument a) => a -> Asm ()
jz :: (Argument a) => a -> Asm ()
jne :: (Argument a) => a -> Asm ()
jnz :: (Argument a) => a -> Asm ()
js :: (Argument a) => a -> Asm ()
jns :: (Argument a) => a -> Asm ()
jg :: (Argument a) => a -> Asm ()
jge :: (Argument a) => a -> Asm ()
jl :: (Argument a) => a -> Asm ()
jle :: (Argument a) => a -> Asm ()
ja :: (Argument a) => a -> Asm ()
jae :: (Argument a) => a -> Asm ()
jb :: (Argument a) => a -> Asm ()
jbe :: (Argument a) => a -> Asm ()
je = ins1 "je"
jz = ins1 "jz"
jne = ins1 "jne"
jnz = ins1 "jnz"
js = ins1 "js"
jns = ins1 "jns"
jg = ins1 "jg"
jge = ins1 "jge"
jl = ins1 "jl"
jle = ins1 "jle"
ja = ins1 "ja"
jae = ins1 "jae"
jb = ins1 "jb"
jbe = ins1 "jbe"

cmpb :: (Argument a, Argument b) => a -> b -> Asm ()
cmpw :: (Argument a, Argument b) => a -> b -> Asm ()
cmpl :: (Argument a, Argument b) => a -> b -> Asm ()
cmpq :: (Argument a, Argument b) => a -> b -> Asm ()
testb :: (Argument a, Argument b) => a -> b -> Asm ()
testw :: (Argument a, Argument b) => a -> b -> Asm ()
testl :: (Argument a, Argument b) => a -> b -> Asm ()
testq :: (Argument a, Argument b) => a -> b -> Asm ()

cmpb = ins2 "cmpb"
cmpw = ins2 "cmpw"
cmpl = ins2 "cmpl"
cmpq = ins2 "cmpq"
testb = ins2 "testb"
testw = ins2 "testw"
testl = ins2 "testl"
testq = ins2 "testq"

sete :: (Argument a) => a -> Asm ()
setne :: (Argument a) => a -> Asm ()
sets :: (Argument a) => a -> Asm ()
setns :: (Argument a) => a -> Asm ()
setg :: (Argument a) => a -> Asm ()
setge :: (Argument a) => a -> Asm ()
setl :: (Argument a) => a -> Asm ()
setle :: (Argument a) => a -> Asm ()
seta :: (Argument a) => a -> Asm ()
setae :: (Argument a) => a -> Asm ()
setb :: (Argument a) => a -> Asm ()
setbe :: (Argument a) => a -> Asm ()

sete = ins1 "sete"
setne = ins1 "setne"
sets = ins1 "sets"
setns = ins1 "setns"
setg = ins1 "setg"
setge = ins1 "setge"
setl = ins1 "setl"
setle = ins1 "setle"
seta = ins1 "seta"
setae = ins1 "setae"
setb = ins1 "setb"
setbe = ins1 "setbe"

dumbAssembly = do
    movq rax rbx
    l <- getLabel
    testq rax rax
    jz l
    l' <- getLabel
    jnz l'
    label l
    addq rax rbx
    ret
    label l'
    subq rax rbx
    ret
