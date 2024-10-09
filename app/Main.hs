module Main (main) where

import Lexing.Lexer

main :: IO ()
main = do
    s <- getLine
    print $ lexString s

