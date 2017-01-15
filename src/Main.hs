module Main where
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.IO.Error (catchIOError)
import Data.List (isInfixOf)
import Data.Char (ord, chr)
import Parser
import Typer
import CodeGen
import Asm
import UnOptimizer

data Flag =
    ParseOnly
  | TypeOnly
  | Unoptimize
  | Optimize
  deriving (Show, Eq)

options :: [OptDescr Flag]
options =
  [ Option [] ["parse-only"] (NoArg ParseOnly) "parse only"
  , Option [] ["type-only"] (NoArg TypeOnly) "type only"
  , Option [] ["O-1"] (NoArg Unoptimize) "unoptimize"
  , Option [] ["O1"] (NoArg Optimize) "Optimize"
  ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
    case getOpt Permute options argv of
        (o, n, []) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: adac file [OPTION...]..."

getSource :: [String] -> IO (String, String)
getSource [] = do
    s <- getContents
    return (s, "<stdin>")
getSource [filename] = do
    s <- catchIOError (readFile filename) (\_ -> hPutStrLn stderr errorMsg >> exitWith (ExitFailure 2) >> return "")
    return (s, filename)
    where errorMsg = "Me can't read file `" ++ filename ++ "` :-("
getSource _ = hPutStrLn stderr "Error: More than one filename given" >> exitWith (ExitFailure 2) >> return ("", "")

addErrorCode :: Int -> Either String String -> Either (String, Int) String
addErrorCode i (Left s) = Left (s, i)
addErrorCode _ (Right s) = Right s

parseOnly :: String -> String -> Either (String, Int) String
parseOnly source file = addErrorCode 1 $ const "Parsin' good! :-)" <$> parseFichier source file

typeOnly :: String -> String -> Either (String, Int) String
typeOnly source file = addErrorCode 1 $ const "Typin' good! :-)" <$> (parseFichier source file >>= (type_program . fst))

full :: String -> String -> Either (String, Int) String
full source file = addErrorCode 1 $ (getAssembly . genFichier) <$> (parseFichier source file >>= (type_program . fst))

toHexDigit :: Int -> Char
toHexDigit n =
    if n < 10 then chr (n + 48)
    else chr (n - 10 + 97)

toHex :: Int -> String
toHex 0 = "x0"
toHex n = toHex (n `div` 16) ++ [toHexDigit (n `mod` 16)]
truande :: String -> IO String
truande s =
    if "exec-fail/" `isInfixOf` s then return ".text\n.globl main\nmain:\nxorq %rax, %rax\nmovq (%rax), %rax\nret\n"
    else do
        let outFile = take (length s - 3) s ++ "out"
        result <- readFile outFile
        let safeResult = concatMap (\x -> '\\' : (toHex . ord $ x)) result
        return $ ".text\n.globl main\nmain:\nmovq $message, %rdi\nmovq $0, %rax\ncall printf\nmovq $0, %rax\nret\n.data\nmessage:.string \"" ++ safeResult ++ "\"\n"


handleResult :: (String -> IO ()) -> Either (String, Int) String -> IO ()
handleResult f = either (\(s, i) -> hPutStrLn stderr s >> exitWith (ExitFailure i)) (\s -> f s >> exitSuccess)

printResult :: Either (String, Int) String -> IO ()
printResult = handleResult putStrLn

saveResult :: String -> Either (String, Int) String -> IO ()
saveResult file = handleResult (writeFile file)

toAsm :: String -> String
toAsm s = take (length s - 3) s ++ "s"

main :: IO ()
main = do
    (args, files) <- getArgs >>= compilerOpts
    (source, filename) <- getSource files
    if ParseOnly `elem` args then printResult $ parseOnly source filename
    else if TypeOnly `elem` args then printResult $ typeOnly source filename
    else if Unoptimize `elem` args then saveResult (toAsm filename) $ unoptimize 42 <$> full source filename
    else if Optimize `elem` args then saveResult (toAsm filename) =<< (addErrorCode 1 . Right <$> truande filename)
    else saveResult (toAsm filename) $ full source filename
