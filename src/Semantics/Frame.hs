module Semantics.Frame
    ( Frame(..)
    ) where

class Frame f where
    newFrame :: String -> [Bool] -> f a
    name :: f a -> String
    formals :: f a -> [a]
    allocLocal :: f a -> Bool -> a