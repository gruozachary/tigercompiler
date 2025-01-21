{-# LANGUAGE TupleSections #-}
module Semantics.Semant
    (
    ) where

import Semantics.SymbolTable
import qualified Parsing.Nodes as N

import Semantics.Analyser
import Control.Monad (void)
import Control.Monad.Trans (lift)
import Control.Monad.State (gets)
import Data.List (sort)
import Data.Functor.Classes (eq1)

eVenv :: (SymbolTable t) => t EnvEntry
eTenv :: (SymbolTable t) => t Ty
eVenv = new
eTenv = fromList [("string", TString), ("int", TInt)]
-- empty venv and tenv

extract :: (a, Analyser t b) -> Analyser t (a, b)
extract (a, mb) = fmap (a,)  mb

transTy :: (SymbolTable t) => N.Type -> Analyser t Ty
transTy = undefined

transExpr :: (SymbolTable t) => N.Expr -> Analyser t Expty
transExpr N.NilEx = return ((), TNil)
transExpr (N.IntEx _) = return ((), TInt)
transExpr (N.StrEx _) = return ((), TString)
transExpr (N.ArrayEx tid size element) = do
    t <- findTy "type of array is not defined" tid
    (et, _) <- expectArray "can't treat non-array as array" t
    case size of
        N.IntEx _ -> return ()
        _ -> lift (Left "size of array is not an integer literal")
    (_, et') <- transExpr element
    matchTwo "array type does not match element initialiser" et et'
    return ((), t)
transExpr (N.RecordEx tid ents) = do
    t <- findTy "type undefined" tid
    stmap <- sort . fst <$> expectRecord "type is not a record" t
    let (is, es) = unzip ents
    es' <- sort . zip (map N.idToStr is) . map snd <$> traverse transExpr es
    matchTwo "record fields invalid" stmap es'
    return ((), t)
transExpr (N.LValEx _) = undefined 
transExpr (N.FunCallEx fid args) = do
    f <- findEnvEntry "function undefined" fid
    (argTs, retT) <- expectFun "cannot call non functions" f
    argTs' <- map snd <$> traverse transExpr args
    matchTwo "function arguments are not of expected type" argTs argTs' 
    return ((), retT)
transExpr (N.NegEx e) = do
    (_, t) <- transExpr e
    expectInt "cannot negate non integers" t
    return ((), t)
    
transExpr (N.OpEx e1 N.AddOp e2) = do
    (_, t1) <- transExpr e1
    expectInt "cannot add non integers" t1
    (_, t2) <- transExpr e2
    expectInt "cannot add non integers" t2
    return ((), TInt)
transExpr (N.OpEx e1 N.SubOp e2) = do
    (_, t1) <- transExpr e1
    expectInt "cannot subtract non integers" t1
    (_, t2) <- transExpr e2
    expectInt "cannot subtract non integers" t2
    return ((), TInt)
transExpr (N.OpEx e1 N.MultOp e2) = do
    (_, t1) <- transExpr e1
    expectInt "cannot multiply non integers" t1
    (_, t2) <- transExpr e2
    expectInt "cannot multiply non integers" t2
    return ((), TInt)
transExpr (N.OpEx e1 N.DivOp e2) = do
    (_, t1) <- transExpr e1
    expectInt "cannot divide non integers" t1
    (_, t2) <- transExpr e2
    expectInt "cannot divide non integers" t2
    return ((), TInt)
transExpr (N.OpEx e1 N.EqOp e2) = do
    (_, t1) <- transExpr e1
    (_, t2) <- transExpr e2
    matchTwo "cannot check for equality on different types" t1 t2
    return ((), TInt)
transExpr (N.OpEx e1 N.NeqOp e2) = do
    (_, t1) <- transExpr e1
    (_, t2) <- transExpr e2
    matchTwo "cannot check for inequality on different types" t1 t2
    return ((), TInt)
transExpr (N.OpEx e1 N.LtOp e2) = do
    (_, t1) <- transExpr e1
    expectInt "cannot compare non integers" t1
    (_, t2) <- transExpr e2
    expectInt "cannot compare non integers" t2
    return ((), TInt)
transExpr (N.OpEx e1 N.LeOp e2) = do
    (_, t1) <- transExpr e1
    expectInt "cannot compare non integers" t1
    (_, t2) <- transExpr e2
    expectInt "cannot compare non integers" t2
    return ((), TInt)
transExpr (N.OpEx e1 N.GtOp e2) = do
    (_, t1) <- transExpr e1
    expectInt "cannot compare non integers" t1
    (_, t2) <- transExpr e2
    expectInt "cannot compare non integers" t2
    return ((), TInt)
transExpr (N.OpEx e1 N.GeOp e2) = do
    (_, t1) <- transExpr e1
    expectInt "cannot compare non integers" t1
    (_, t2) <- transExpr e2
    expectInt "cannot compare non integers" t2
    return ((), TInt)
transExpr (N.OpEx e1 N.AndOp e2) = do
    (_, t1) <- transExpr e1
    expectInt "cannot AND non integers" t1
    (_, t2) <- transExpr e2
    expectInt "cannot AND non integers" t2
    return ((), TInt)
transExpr (N.OpEx e1 N.OrOp e2) = do
    (_, t1) <- transExpr e1
    expectInt "cannot OR non integers" t1
    (_, t2) <- transExpr e2
    expectInt "cannot OR non integers" t2
    return ((), TInt)




transExpr (N.Exs _) = undefined
transExpr (N.AssignEx _ _ ) = undefined
transExpr (N.IfEx _ _ _) = undefined
transExpr (N.WhileEx _ _) = undefined
transExpr (N.ForEx _ _ _ _) = undefined
transExpr (N.BreakEx) = return ((), TUnit)
transExpr (N.LetEx _ _) = undefined

transChunk :: (SymbolTable t) => N.Chunk -> Analyser t ()
transChunk = undefined

analyse :: (SymbolTable t) => N.Program -> Analyser t ()
analyse (N.ExprProg e)       = void (transExpr e)
analyse (N.ChunkProg [])     = return ()
analyse (N.ChunkProg (c:cs)) = transChunk c >> analyse (N.ChunkProg cs)
