module Semantics.Semant
    (
    ) where

import Semantics.SymbolTable
import qualified Parsing.Nodes as N

import Semantics.Analyser
import Control.Monad (void)
import Control.Monad.Trans (lift)
import Data.List (sort)

eVenv :: (SymbolTable t) => t EnvEntry
eTenv :: (SymbolTable t) => t Ty
eVenv = new
eTenv = fromList [("string", TString), ("int", TInt)]
-- empty venv and tenv

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
transExpr (N.LValEx _) = undefined  -- TODO
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
transExpr (N.OpEx e1 op e2) = do
    l <- transExpr e1
    r <- transExpr e2
    transOp op l r
transExpr (N.Exs es) = transExpr (last es) -- es will never be empty
transExpr (N.AssignEx _ _) = undefined -- TODO
transExpr (N.IfEx p b1 mb2) = do
    (_, pT) <- transExpr p
    expectInt "can only calculate truthiness of integers" pT
    (_, t1) <- transExpr b1
    case mb2 of
        Just b2 -> do
            (_, t2) <- transExpr b2
            matchTwo "both branches of an if must produce the same type" t1 t2
        Nothing -> return ()
    return ((), t1)
transExpr (N.WhileEx _ _) = undefined
transExpr (N.ForEx _ _ _ _) = undefined
transExpr (N.BreakEx) = return ((), TUnit)
transExpr (N.LetEx _ _) = undefined

transChunk :: (SymbolTable t) => N.Chunk -> Analyser t ()
transChunk = undefined

transOp :: N.Op -> Expty -> Expty -> Analyser t Expty
transOp N.AddOp (_, lt) (_, rt) = do
    expectInt "cannot add non-integers" lt
    expectInt "cannot add non-integers" rt
    return ((), TInt)
transOp N.SubOp (_, lt) (_, rt) = do
    expectInt "cannot subtract non-integers" lt
    expectInt "cannot subtract non-integers" rt
    return ((), TInt)
transOp N.MultOp (_, lt) (_, rt) = do
    expectInt "cannot multiply non-integers" lt
    expectInt "cannot multiply non-integers" rt
    return ((), TInt)
transOp N.DivOp (_, lt) (_, rt) = do
    expectInt "cannot divide non-integers" lt
    expectInt "cannot divide non-integers" rt
    return ((), TInt)
transOp N.EqOp (_, lt) (_, rt) = do
    matchTwo "cannot check for equality on different types" lt rt
    return ((), TInt)
transOp N.NeqOp (_, lt) (_, rt) = do
    matchTwo "cannot check for inequality on different types" lt rt
    return ((), TInt)
transOp N.LtOp (_, lt) (_, rt) = do
    expectInt "cannot compare non-integers" lt
    expectInt "cannot compare non-integers" rt
    return ((), TInt)
transOp N.LeOp (_, lt) (_, rt) = do
    expectInt "cannot compare non-integers" lt
    expectInt "cannot compare non-integers" rt
    return ((), TInt)
transOp N.GtOp (_, lt) (_, rt) = do
    expectInt "cannot compare non-integers" lt
    expectInt "cannot compare non-integers" rt
    return ((), TInt)
transOp N.GeOp (_, lt) (_, rt) = do
    expectInt "cannot compare non-integers" lt
    expectInt "cannot compare non-integers" rt
    return ((), TInt)
transOp N.AndOp (_, lt) (_, rt) = do
    expectInt "cannot AND non-integers" lt
    expectInt "cannot AND non-integers" rt
    return ((), TInt)
transOp N.OrOp (_, lt) (_, rt) = do
    expectInt "cannot OR non-integers" lt
    expectInt "cannot OR non-integers" rt
    return ((), TInt)

analyse :: (SymbolTable t) => N.Program -> Analyser t ()
analyse (N.ExprProg e)       = void (transExpr e)
analyse (N.ChunkProg [])     = return ()
analyse (N.ChunkProg (c:cs)) = transChunk c >> analyse (N.ChunkProg cs)
