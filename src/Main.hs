module Main where
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.IO.Error (catchIOError)
import Data.Maybe (fromMaybe)
import Parser
import Typer

data Flag =
    ParseOnly
  | TypeOnly
  deriving (Show, Eq)

options :: [OptDescr Flag]
options = [
    Option [] ["parse-only"] (NoArg ParseOnly) "parse only",
    Option [] ["type-only"] (NoArg TypeOnly) "type only"
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
addErrorCode i (Right s) = Right s

parseOnly :: String -> String -> Either (String, Int) String
parseOnly source file = addErrorCode 1 $ fmap (const "Parsin' good! :-)") $ parseFichier source file

typeOnly :: String -> String -> Either (String, Int) String
typeOnly source file = addErrorCode 1 $ fmap (const "Typin' good! :-)") $ parseFichier source file >>= (type_program . fst)

main :: IO ()
main = do
    (args, files) <- getArgs >>= compilerOpts
    (source, filename) <- getSource files
    let res = if ParseOnly `elem` args then parseOnly source filename
              else if TypeOnly `elem` args then typeOnly source filename
              else Left ("Halp! Me dunno wat to do!", 2)
    either (\(s, i) -> hPutStrLn stderr s >> exitWith (ExitFailure i))
           (\s -> putStrLn s >> exitWith ExitSuccess)
           res
