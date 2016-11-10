module Main where
import Parser
import Data.Char (toLower)

main :: IO ()
main = do
    s <- getContents
    print $ parseFichier (map toLower s) "<stdin>"
