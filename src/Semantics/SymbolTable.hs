{-# LANGUAGE DeriveFunctor #-}
module Semantics.SymbolTable
    ( SymbolTable, new, insert, look, entries, add, fromList
    ) where

import qualified Data.Map as Map

class SymbolTable t where
    -- provides an empty table
    new :: t a
    
    -- initialise a symbol table from a list of bindings
    fromList :: [(String, a)] -> t a
    fromList = foldr (\(k, x) m -> insert m k x) new
    
    -- inserts an element into the table, OVERWRITING existing elements if the
    -- Stringey already exists
    insert :: t a -> String -> a -> t a
    
    -- returns the value associated with a Stringey
    look :: t a -> String -> Maybe a
    
    -- returns the set of entries in the table
    entries :: t a -> [(String, a)]
    
    -- combines two symbol tables, existing elements in t0 being overwritten by
    -- elements in t1
    add :: t a -> t a -> t a
    add t0 = foldr (\(k, x) t -> insert t k x) t0 . entries

newtype MST v = MST (Map.Map String v) deriving Functor

instance SymbolTable MST where
    new = MST Map.empty

    insert (MST m) k a = MST $ Map.insert k a m
    look (MST m) k = Map.lookup k m
    entries (MST m) = Map.toList m
    add (MST m1) (MST m2) = MST (Map.union m1 m2)

-- TODO: add Trie implementation