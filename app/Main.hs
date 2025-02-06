module Main (main) where

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
        Left  e -> ioError (userError e)

    case runSemant p of
        Right _ -> putStrLn "program type checks!"
        Left  e -> ioError (userError e)
    
    print p
