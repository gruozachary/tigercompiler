module Main (main) where

import Lexing.Lexer
import Parsing.Parser

main :: IO ()
main = do
    s <- getLine
    print (runAlex s parse)

