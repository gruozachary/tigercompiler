module Semantics.Semant
    (
    ) where

import Semantics.SymbolTable
import qualified Parsing.Nodes as N

import Semantics.Analyser
import Control.Monad (void)
import Control.Monad.Trans (lift)
import Data.List (sort)
import Data.Foldable (traverse_)
import Control.Monad.State (put, get)
import Parsing.Nodes (TyFields(TyFields))

eVenv :: (SymbolTable t) => t EnvEntry
eTenv :: (SymbolTable t) => t Ty
eVenv = new
eTenv = fromList [("string", TString), ("int", TInt)]
-- empty venv and tenv

transTy :: (SymbolTable t) => N.Type -> Analyser t Ty
transTy (N.IdTy tyid) = findTy "type not found" tyid
transTy (N.RecordTy (N.TyFields tyfields)) = do
    let (is, tids) = unzip tyfields
    tys <- traverse (findTy "could not find type") tids
    TRecord (zip (map N.idToStr is) tys) <$> getNextId
transTy (N.ArrayTy tyid) = do
    ty <- findTy "could not find type" tyid
    TArray ty <$> getNextId


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
transExpr (N.LValEx lv) = transLValue lv
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
transExpr (N.AssignEx lv e) = do
    (_, lvT) <- transLValue lv
    (_, eT)  <- transExpr e
    matchTwo "value of RHS does not match LHS" eT lvT
    return ((), TUnit)
transExpr (N.IfEx p b1 mb2) = do
    (_, pT) <- transExpr p
    expectInt "can only calculate truthiness of integers" pT
    (_, t1) <- transExpr b1
    case mb2 of
        Just b2 -> do
            (_, t2) <- transExpr b2
            matchTwo "both branches of an if must produce the same type" t1 t2
            return ((), t1)
        Nothing -> do
            expectUnit "type of if must be unit" t1
            return ((), TUnit)
transExpr (N.WhileEx p bd) = do
    (_, pT) <- transExpr p
    expectInt "can only calculate truthiness of integers" pT
    (_, bdT) <- transExpr bd
    expectUnit "body of while can't return a value" bdT
    return ((), TUnit)
transExpr (N.ForEx _ ini ub bd) = do
    (_, iniT) <- transExpr ini
    expectInt "initialiser must be integer" iniT
    (_, ubT) <- transExpr ub
    expectInt "upper bound must be integer" ubT
    (_, bdT) <- transExpr bd
    expectInt "for body must not return anything" bdT
    return ((), TUnit)
transExpr N.BreakEx = return ((), TUnit)
transExpr (N.LetEx cs []) = transChunks cs >> return ((), TUnit)
transExpr (N.LetEx cs bd) = transChunks cs >> transExpr (last bd)


transLValue :: (SymbolTable t) => N.LValue -> Analyser t Expty
transLValue (N.IdLV i) = do
    envEntry <- findEnvEntry "variable not found" i
    case envEntry of
        (VarEntry t) -> return ((), t)
        _ -> lift (Left "cannot modify a function")
transLValue (N.RecLV lvalue (N.Id i)) = do
    (_, recordT) <- transLValue lvalue
    (pairs, _) <- expectRecord "cannot get member from a non-record" recordT
    case lookup i pairs of
        Nothing -> lift (Left "cannot get members of nothing")
        (Just ty) -> return ((), ty) 

transLValue (N.ArrLV lvalue e) = do
    (_, arrayT) <- transLValue lvalue
    (elementT, _) <- expectArray "cannot index a non-array" arrayT
    (_, eT) <- transExpr e
    expectInt "array index must be an integer" eT
    return ((), elementT)
    

transChunk :: (SymbolTable t) => N.Chunk -> Analyser t ()
transChunk (N.TypeChunk []) = return ()
transChunk (N.TypeChunk ((N.TypeDecl (N.Id i) t):ts)) = do
    ty <- transTy t
    state <- get
    put (state { currentTyEnv = insert (currentTyEnv state) i ty })
    transChunk (N.TypeChunk ts)
transChunk (N.FunChunk []) = return ()
transChunk (N.FunChunk ((N.Function (N.Id fid) (TyFields params) maybeRetT body) : fs)) = do
    (_, retT') <- transExpr body
    state <- get
    
    case maybeRetT of
        Just tid -> do
            retT <- findTy "cannot find return type" tid
            matchTwo "actual return type does not match provided" retT retT'
        Nothing -> return ()
    paramTys <- traverse (findTy "paramater type not found" . snd) params
    put (state { currentEnv = insert (currentEnv state) fid (FunEntry paramTys retT') })
transChunk (N.FunChunk ((N.Primitive (N.Id fid) (TyFields params) maybeRetT) : fs)) = do
    state <- get
    case maybeRetT of
        Just tid -> do
            retT <- findTy "cannot find return type" tid
            paramTys <- traverse (findTy "paramater type not found" . snd) params
            put (state { currentEnv = insert (currentEnv state) fid (FunEntry paramTys retT) })
        Nothing -> lift (Left "unknown primitive")
transChunk (N.VarChunk (N.VarDecl (N.Id i) maybeT e)) = do
    (_, t) <- transExpr e
    case maybeT of
        Just tyId -> do
            t' <- findTy "return type does not exist" tyId
            matchTwo "inferred type does not match actual type" t t'
        Nothing -> return ()
    state <- get
    put (state { currentEnv = insert (currentEnv state) i (VarEntry t) })


transChunk (N.ImportChunk _) = lift (Left "you should not be importing...")

transChunks :: (SymbolTable t) => [N.Chunk] -> Analyser t ()
transChunks = traverse_ transChunk

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
