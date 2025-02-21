module Semantics.Semant
    ( runSemant
    ) where

import Semantics.SymbolTable
import qualified Parsing.Nodes as N

import Semantics.Analyser
import Control.Monad (void)
import Data.List (sort)
import Data.Foldable (traverse_)
import Parsing.Nodes (TyFields(TyFields))
import Control.Monad.RWS (runRWS)

eVenv :: (SymbolTable t) => t EnvEntry
eTenv :: (SymbolTable t) => t Ty
eVenv = new
eTenv = fromList [("string", TString), ("int", TInt)]
-- empty venv and tenv

transTy :: (SymbolTable t) => N.Type -> Analyser t Ty
transTy (N.IdTy tyid) = snd <$> findTy "type not found" tyid (\t -> return ((), t))
transTy (N.RecordTy (N.TyFields tyfields)) = do
    let (is, tids) = unzip tyfields
    tys <- traverse (\tid -> snd <$> findTy "could not find type" tid (\t -> return ((), t))) tids
    TRecord (zip (map N.idToStr is) tys) <$> getNextId
transTy (N.ArrayTy tyid) = do
    (_, ty) <- findTy "could not find type" tyid $ \ty -> return ((), ty)
    TArray ty <$> getNextId


transExpr :: (SymbolTable t) => N.Expr -> Analyser t Expty
transExpr N.NilEx = return ((), TNil)
transExpr (N.IntEx _) = return ((), TInt)
transExpr (N.StrEx _) = return ((), TString)
transExpr (N.ArrayEx tid size element) = do
    findTy "type of array is not defined" tid $ \t -> do
        expectArray "can't treat non-array as array" t $ \ets _ -> do
            case size of
                N.IntEx _ -> return ()
                _ -> err "size of array is not an integer literal"
            (_, ets') <- transExpr element
            matchTwo "array type does not match element initialiser" ets ets' $
                return ((), t)
transExpr (N.RecordEx tid ents) = do
    findTy "type undefined" tid $ \t -> do
        expectRecord "type is not a record" t $ \recordElem _ -> do
            let stmap = sort recordElem
            let (is, es) = unzip ents
            es' <- sort . zip (map N.idToStr is) . map snd <$> traverse transExpr es
            matchTwo "record fields invalid" stmap es' $
                return ((), t)
transExpr (N.LValEx lv) = transLValue lv
transExpr (N.FunCallEx fid args) = do
    findEnvEntry "function undefined" fid $ \f -> do
            expectFun "cannot call non functions" f $ \argTs retT -> do
                argTs' <- map snd <$> traverse transExpr args
                matchTwo "function arguments are not of expected type" argTs argTs' $
                    return ((), retT)
transExpr (N.NegEx e) = do
    (_, t) <- transExpr e
    expectInt "cannot negate non integers" t $
        return ((), t)
transExpr (N.OpEx e1 op e2) = do
    l <- transExpr e1
    r <- transExpr e2
    transOp op l r
transExpr (N.Exs es) = transExpr (last es) -- es will never be empty
transExpr (N.AssignEx lv e) = do
    (_, lvT) <- transLValue lv
    (_, eT)  <- transExpr e
    matchTwo "value of RHS does not match LHS" eT lvT $
        return ((), TUnit)
transExpr (N.IfEx p b1 mb2) = do
    (_, pT) <- transExpr p
    expectInt "can only calculate truthiness of integers" pT $ do
        (_, t1) <- transExpr b1
        case mb2 of
            Just b2 -> do
                (_, t2) <- transExpr b2
                matchTwo "both branches of an if must produce the same type" t1 t2 $
                    return ((), t1)
            Nothing -> do
                expectUnit "type of if must be unit" t1 $
                    return ((), TUnit)
transExpr (N.WhileEx p bd) = do
    (_, pT) <- transExpr p
    expectInt "can only calculate truthiness of integers" pT $ do
        (_, bdT) <- transExpr bd
        expectUnit "body of while can't return a value" bdT $
            return ((), TUnit)
transExpr (N.ForEx _ ini ub bd) = do
    (_, iniT) <- transExpr ini
    expectInt "initialiser must be integer" iniT $ do
        (_, ubT) <- transExpr ub
        expectInt "upper bound must be integer" ubT $ do
            (_, bdT) <- transExpr bd
            expectInt "for body must not return anything" bdT $
                return ((), TUnit)
transExpr N.BreakEx = return ((), TUnit)
transExpr (N.LetEx cs []) = transChunks cs $ return ((), TUnit)
transExpr (N.LetEx cs bd) = transChunks cs >> transExpr (last bd)


transLValue :: (SymbolTable t) => N.LValue -> Analyser t Expty
transLValue (N.IdLV i) = do
    findEnvEntry "variable not found" i $ \envEntry ->
        case envEntry of
            (VarEntry t) -> return ((), t)
            _ -> errFail "cannot modify a function"
transLValue (N.RecLV lvalue (N.Id i)) = do
    (_, recordT) <- transLValue lvalue
    expectRecord "cannot get member from a non-record" recordT $ \pairs _ ->
        case lookup i pairs of
            Nothing -> errFail "cannot get members of nothing"
            Just ty -> return ((), ty)

transLValue (N.ArrLV lvalue e) = do
    (_, arrayT) <- transLValue lvalue
    expectArray "cannot index a non-array" arrayT $ \elementT _ -> do
        (_, eT) <- transExpr e
        expectInt "array index must be an integer" eT $
            return ((), elementT)


transChunk :: (SymbolTable t) => N.Chunk -> Analyser t a -> Analyser t a
transChunk (N.TypeChunk []) f = f
transChunk (N.TypeChunk ((N.TypeDecl i t):ts)) f = do
    ty <- transTy t
    addType (N.idToTyId i) ty $
        transChunk (N.TypeChunk ts) f
transChunk (N.FunChunk []) f = f
transChunk (N.FunChunk ((N.Function fid (TyFields params) maybeRetT body) : fs)) f = do
    (_, retT') <- transExpr body

    case maybeRetT of
        Just tid -> do
            findTy "cannot find return type" tid $ \retT -> do
                matchTwo "actual return type does not match provided" retT retT' $
                    return ((), TUnknown)
        Nothing -> return ((), TUnknown)

    paramTys <- traverse (\(_, p) -> findTy "parameter type not found" p (\t -> return ((), t))) params

    -- TODO: might change with mutual recursion
    addFun fid (map snd paramTys) retT' $
        transChunk (N.FunChunk fs) f
transChunk (N.FunChunk ((N.Primitive fid (TyFields params) maybeRetT) : fs)) f = do
    case maybeRetT of
        Just tid -> void $
            findTy "cannot find return type" tid $ \retT -> do
                paramTys <- traverse (\(_, p) -> findTy "parameter type not found" p (\t -> return ((), t))) params
                addFun fid (map snd paramTys) retT $
                    transChunk (N.FunChunk fs) f
                return ((), TUnknown)
        Nothing -> err "unknown primitive"
transChunk (N.VarChunk (N.VarDecl i maybeT e)) f = do
    (_, t) <- transExpr e
    case maybeT of
        Just tyId -> void $
            findTy "return type does not exist" tyId $ \t' ->
                matchTwo "inferred type does not match actual type" t t' $
                    addVar i t $ f >> return ((), TUnknown)
        Nothing -> return ()


transChunk (N.ImportChunk _) floor = err "you should not be importing..."

transChunks :: (SymbolTable t) => [N.Chunk] -> Analyser t a -> Analyser t a
transChunks cs f = foldr transChunk f cs

transOp :: N.Op -> Expty -> Expty -> Analyser t Expty
transOp N.AddOp (_, lt) (_, rt) = do
    expectInt "cannot add non-integers" lt $
        expectInt "cannot add non-integers" rt $
            return ((), TInt)
transOp N.SubOp (_, lt) (_, rt) = do
    expectInt "cannot subtract non-integers" lt $
        expectInt "cannot subtract non-integers" rt $
            return ((), TInt)
transOp N.MultOp (_, lt) (_, rt) = do
    expectInt "cannot multiply non-integers" lt $
        expectInt "cannot multiply non-integers" rt $
            return ((), TInt)
transOp N.DivOp (_, lt) (_, rt) = do
    expectInt "cannot divide non-integers" lt $
        expectInt "cannot divide non-integers" rt $
            return ((), TInt)
transOp N.EqOp (_, lt) (_, rt) = do
    matchTwo "cannot check for equality on different types" lt rt $
        return ((), TInt)
transOp N.NeqOp (_, lt) (_, rt) = do
    matchTwo "cannot check for inequality on different types" lt rt $
        return ((), TInt)
transOp N.LtOp (_, lt) (_, rt) = do
    expectInt "cannot compare non-integers" lt $
        expectInt "cannot compare non-integers" rt $
            return ((), TInt)
transOp N.LeOp (_, lt) (_, rt) = do
    expectInt "cannot compare non-integers" lt $
        expectInt "cannot compare non-integers" rt $
            return ((), TInt)
transOp N.GtOp (_, lt) (_, rt) = do
    expectInt "cannot compare non-integers" lt $
        expectInt "cannot compare non-integers" rt $
            return ((), TInt)
transOp N.GeOp (_, lt) (_, rt) = do
    expectInt "cannot compare non-integers" lt $
        expectInt "cannot compare non-integers" rt $
            return ((), TInt)
transOp N.AndOp (_, lt) (_, rt) = do
    expectInt "cannot AND non-integers" lt $
        expectInt "cannot AND non-integers" rt $
            return ((), TInt)
transOp N.OrOp (_, lt) (_, rt) = do
    expectInt "cannot OR non-integers" lt $
        expectInt "cannot OR non-integers" rt $
            return ((), TInt)

analyse :: (SymbolTable t) => N.Program -> Analyser t ()
analyse (N.ExprProg e)       = void (transExpr e)
analyse (N.ChunkProg [])     = return ()
analyse (N.ChunkProg (c:cs)) = transChunk c >> analyse (N.ChunkProg cs)

runSemant :: N.Program -> Either String ()
-- runSemant p = evalStateT (analyse p) initialData
--     where initialData = Data { currentEnv   = eVenv :: MST EnvEntry
--                              , currentTyEnv = eTenv :: MST Ty
--                              , nextId       = 0                     }
runSemant = runRWS