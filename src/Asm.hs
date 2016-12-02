module Asm where
import Data.Char (toLower)

data Register =
    Rax | Rbx | Rcx | Rdx | Rsi | Rdi | Rbp | Rsp | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  | Eax | Ebx | Ecx | Edx | Esi | Edi | Ebp | Esp | R8d | R9d | R10d | R11d | R12d | R13d | R14d | R15d
  | Ax  | Bx  | Cx  | Dx  | Si  | Di  | Bp  | Sp  | R8w | R9w | R10w | R11w | R12w | R13w | R14w | R15w
  | Al  | Bl  | Cl  | Dl  | Ah  | Bh  | Ch  | Dh  | Sil | Dil | Bpl  | Spl  | R8b  | R9b  | R10b | R11b | R12b | R13b | R14b | R15b
    deriving (Show)

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

instance Argument Register where
    arg r = case (show r) of
              x:xs -> '%':(toLower x):xs
              [] -> "%" -- shouldn't happen

ins0 :: String -> String
ins0 s = s ++ "\n"

ins1 :: (Argument a) => String -> a -> String
ins1 s r = s ++ " " ++ arg r ++ "\n"

ins2 :: (Argument a, Argument b) => String -> a -> b -> String
ins2 s r1 r2 = s ++ " " ++ arg r1 ++ " " ++ arg r2 ++ "\n"

-- TODO: see if there is a way not to give the type...
movb :: (Argument a, Argument b) => a -> b -> String
movw :: (Argument a, Argument b) => a -> b -> String
movl :: (Argument a, Argument b) => a -> b -> String
movq :: (Argument a, Argument b) => a -> b -> String
movb = ins2 "movb"
movw = ins2 "movw"
movl = ins2 "movl"
movq = ins2 "movq"

-- TODO: the first or second argument should be an integer
movabsq :: (Argument a, Argument b) => a -> b -> String
movsbw :: (Argument a, Argument b) => a -> b -> String
movsbl :: (Argument a, Argument b) => a -> b -> String
movsbq :: (Argument a, Argument b) => a -> b -> String
movswl :: (Argument a, Argument b) => a -> b -> String
movswq :: (Argument a, Argument b) => a -> b -> String
movslq :: (Argument a, Argument b) => a -> b -> String
movzbw :: (Argument a, Argument b) => a -> b -> String
movzbl :: (Argument a, Argument b) => a -> b -> String
movzbq :: (Argument a, Argument b) => a -> b -> String
movzwl :: (Argument a, Argument b) => a -> b -> String
movzwq :: (Argument a, Argument b) => a -> b -> String
leab :: (Argument a, Argument b) => a -> b -> String
leaw :: (Argument a, Argument b) => a -> b -> String
leal :: (Argument a, Argument b) => a -> b -> String
leaq :: (Argument a, Argument b) => a -> b -> String
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

incb :: (Argument a) => a -> String
incw :: (Argument a) => a -> String
incl :: (Argument a) => a -> String
incq :: (Argument a) => a -> String
decb :: (Argument a) => a -> String
decw :: (Argument a) => a -> String
decl :: (Argument a) => a -> String
decq :: (Argument a) => a -> String
negb :: (Argument a) => a -> String
negw :: (Argument a) => a -> String
negl :: (Argument a) => a -> String
negq :: (Argument a) => a -> String
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

addb :: (Argument a, Argument b) => a -> b -> String
addw :: (Argument a, Argument b) => a -> b -> String
addl :: (Argument a, Argument b) => a -> b -> String
addq :: (Argument a, Argument b) => a -> b -> String
subb :: (Argument a, Argument b) => a -> b -> String
subw :: (Argument a, Argument b) => a -> b -> String
subl :: (Argument a, Argument b) => a -> b -> String
subq :: (Argument a, Argument b) => a -> b -> String
imulw :: (Argument a, Argument b) => a -> b -> String
imull :: (Argument a, Argument b) => a -> b -> String
imulq :: (Argument a, Argument b) => a -> b -> String

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

idivq :: (Argument a) => a -> String
cqto :: String
idivq = ins1 "idivq"
cqto = ins0 "cqto"

notb :: (Argument a) => a -> String
notw :: (Argument a) => a -> String
notl :: (Argument a) => a -> String
notq :: (Argument a) => a -> String

notb = ins1 "notb"
notw = ins1 "notw"
notl = ins1 "notl"
notq = ins1 "notq"


andb :: (Argument a, Argument b) => a -> b -> String
andw :: (Argument a, Argument b) => a -> b -> String
andl :: (Argument a, Argument b) => a -> b -> String
andq :: (Argument a, Argument b) => a -> b -> String
orb :: (Argument a, Argument b) => a -> b -> String
orw :: (Argument a, Argument b) => a -> b -> String
orl :: (Argument a, Argument b) => a -> b -> String
orq :: (Argument a, Argument b) => a -> b -> String
xorb :: (Argument a, Argument b) => a -> b -> String
xorw :: (Argument a, Argument b) => a -> b -> String
xorl :: (Argument a, Argument b) => a -> b -> String
xorq :: (Argument a, Argument b) => a -> b -> String
shlb :: (Argument a, Argument b) => a -> b -> String
shlw :: (Argument a, Argument b) => a -> b -> String
shll :: (Argument a, Argument b) => a -> b -> String
shlq :: (Argument a, Argument b) => a -> b -> String
shrb :: (Argument a, Argument b) => a -> b -> String
shrw :: (Argument a, Argument b) => a -> b -> String
shrl :: (Argument a, Argument b) => a -> b -> String
shrq :: (Argument a, Argument b) => a -> b -> String
sarb :: (Argument a, Argument b) => a -> b -> String
sarw :: (Argument a, Argument b) => a -> b -> String
sarl :: (Argument a, Argument b) => a -> b -> String
sarq :: (Argument a, Argument b) => a -> b -> String

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

jmp :: (Argument a) => a -> String
call :: (Argument a) => a -> String
jmp = ins1 "jmp"
call = ins1 "jmp"

leave :: String
ret :: String
leave = ins0 "leave"
ret = ins0 "ret"

je :: (Argument a) => a -> String
jz :: (Argument a) => a -> String
jne :: (Argument a) => a -> String
jnz :: (Argument a) => a -> String
js :: (Argument a) => a -> String
jns :: (Argument a) => a -> String
jg :: (Argument a) => a -> String
jge :: (Argument a) => a -> String
jl :: (Argument a) => a -> String
jle :: (Argument a) => a -> String
ja :: (Argument a) => a -> String
jae :: (Argument a) => a -> String
jb :: (Argument a) => a -> String
jbe :: (Argument a) => a -> String
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

cmpb :: (Argument a, Argument b) => a -> b -> String
cmpw :: (Argument a, Argument b) => a -> b -> String
cmpl :: (Argument a, Argument b) => a -> b -> String
cmpq :: (Argument a, Argument b) => a -> b -> String
testb :: (Argument a, Argument b) => a -> b -> String
testw :: (Argument a, Argument b) => a -> b -> String
testl :: (Argument a, Argument b) => a -> b -> String
testq :: (Argument a, Argument b) => a -> b -> String

cmpb = ins2 "cmpb"
cmpw = ins2 "cmpw"
cmpl = ins2 "cmpl"
cmpq = ins2 "cmpq"
testb = ins2 "testb"
testw = ins2 "testw"
testl = ins2 "testl"
testq = ins2 "testq"

sete :: (Argument a) => a -> String
setne :: (Argument a) => a -> String
sets :: (Argument a) => a -> String
setns :: (Argument a) => a -> String
setg :: (Argument a) => a -> String
setge :: (Argument a) => a -> String
setl :: (Argument a) => a -> String
setle :: (Argument a) => a -> String
seta :: (Argument a) => a -> String
setae :: (Argument a) => a -> String
setb :: (Argument a) => a -> String
setbe :: (Argument a) => a -> String

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

label :: String -> String
label s = s ++ ":\n"

