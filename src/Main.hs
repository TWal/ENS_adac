module Main where
import Parser

main :: IO ()
main = do
    s <- getContents
    print $ parseExp s "<stdin>"
