module Main (main) where

import Data.Foldable (traverse_)

import Lexing.Lexer
import Parsing.Parser
import Semantics.Semant
import System.Environment (getArgs)

main :: IO ()
main = do
    (a:_) <- getArgs
    c <- readFile a

    p <- case runAlex c parse of
        Right p -> pure p
        Left  e -> fail e

    case runSemant p of
        [] -> putStrLn "program type checks!"
        es -> traverse_ fail es
    
    print p
