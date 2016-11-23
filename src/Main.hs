module Main where
import Parser
import Typer

error_either :: String -> Either String a -> IO a
error_either pr (Left s)  = fail $ pr ++ " : " ++ s
error_either _  (Right x) = return x

main :: IO ()
main = do
    s <- getContents
    f <- error_either "parser" $ parseFichier s "<stdin>"
    t <- error_either "typer" $ type_program $ fst f
    return ()

