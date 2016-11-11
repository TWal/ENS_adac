module Main where
import Parser

main :: IO ()
main = do
    s <- getContents
    print $ parseFichier s "<stdin>"
