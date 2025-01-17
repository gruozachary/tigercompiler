module Semantics.SymbolTable
    ( SymbolTable, new, insert, look, entries, add
    ) where

class SymbolTable t where
    -- provides an empty table
    new :: t k a
    -- inserts an element into the table, OVERWRITING existing elements if the
    -- key already exists
    insert :: t k a -> k -> a -> t k a
    -- returns the value associated with a key
    look :: t k a -> k -> a
    -- returns the set of entries in the table
    entries :: t k a -> [(k, a)]
    -- combines to symbol tables, existing elements in t0 being overwritten by
    -- elements in t1
    add :: t k a -> t k a -> t k a
    add t0 = foldr (\(k, x) t -> insert t k x) t0 . entries

-- TODO: add Map implementation
-- TODO: add Trie implementation