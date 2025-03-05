module Semantics.Semant
    ( runSemant
    ) where

import Semantics.SymbolTable
import qualified Parsing.Nodes as N

import Semantics.Analyser
import Control.Monad (void)
import Data.List (sort)
import Parsing.Nodes (TyFields(TyFields))
import Control.Monad.RWS (runRWS)
import Data.Foldable(traverse_)

eVenv :: (SymbolTable t) => t EnvEntry
eTenv :: (SymbolTable t) => t Ty
eVenv = fromList
    [ ("chr", FunEntry [TInt] TString)
    , ("concat", FunEntry [TString, TString] TString)
    , ("exit", FunEntry [TInt] TUnit)
    , ("flush", FunEntry [] TUnit)
    , ("getchar", FunEntry [] TString)
    , ("not", FunEntry [TInt] TInt)
    , ("ord", FunEntry [TString] TInt)
    , ("print", FunEntry [TString] TUnit)
    , ("print_err", FunEntry [TString] TUnit)
    , ("print_int", FunEntry [TInt] TUnit)
    , ("size", FunEntry [TString] TInt)
    , ("strcmp", FunEntry [TString, TString] TInt)
    , ("streq", FunEntry [TString, TString] TInt)
    , ("substring", FunEntry [TString, TInt, TInt] TString) ]
    
eTenv = fromList [("string", TString), ("int", TInt)]
-- empty venv and tenv

transTy :: (SymbolTable t) => N.Type -> Analyser t Ty
transTy (N.IdTy tyid) = snd <$> findTy tyid (erf "type not found") (\t -> return ((), t))
transTy (N.RecordTy (N.TyFields tyfields)) = do
    let (is, tids) = unzip tyfields
    tys <- traverse (\tid -> snd <$> findTy  tid (erf "could not find type") (\t -> return ((), t))) tids
    TRecord (zip (map N.idToStr is) tys) <$> getNextId
transTy (N.ArrayTy tyid) = do
    (_, ty) <- findTy tyid (erf "could not find type") $ \ty -> return ((), ty)
    TArray ty <$> getNextId


transExpr :: (SymbolTable t) => N.Expr -> Analyser t Expty
transExpr N.NilEx = return ((), TNil)
transExpr (N.IntEx _) = return ((), TInt)
transExpr (N.StrEx _) = return ((), TString)
transExpr (N.ArrayEx tid sizeExpr element) = do
    findTy tid (erf "type of array is not defined") $ \t -> do
        expectArray t (erf "can't treat non-array as array") $ \ets _ -> do
            (_, sizeT) <- transExpr sizeExpr
            expectInt sizeT (erf "size of array is not an integer") $ do
                (_, ets') <- transExpr element
                matchTwoElements ets ets' (erf "array type does not match element initialiser") $
                    return ((), t)

transExpr (N.RecordEx tid ents) = do
    findTy tid (erf "type undefined") $ \t -> do
        expectRecord t (erf "type is not a record") $ \recordElem _ -> do
            let stmap = sort recordElem
            let (is, es) = unzip ents
            es' <- sort . zip (map N.idToStr is) . map snd <$> traverse transExpr es
            matchTwo stmap es' (erf "record fields invalid") $
                return ((), t)
transExpr (N.LValEx lv) = transLValue lv
transExpr (N.FunCallEx fid args) = do
    findEnvEntry fid (erf "function not found") $ \f -> do
            expectFun f (erf "cannot call non functions") $ \argTs retT -> do
                argTs' <- map snd <$> traverse transExpr args
                matchTwo argTs argTs' (erf "function arguments are not of expected type") $
                    return ((), retT)
transExpr (N.NegEx e) = do
    (_, t) <- transExpr e
    expectInt t (erf "cannot negate non integers") $
        return ((), t)
transExpr (N.OpEx e1 op e2) = do
    l <- transExpr e1
    r <- transExpr e2
    transOp op l r
transExpr (N.Exs []) = return ((), TUnit)
transExpr (N.Exs es) = traverse_ transExpr (init es) >> transExpr (last es)--  transExpr (last es) -- es will never be empty
transExpr (N.AssignEx lv e) = do
    (_, lvT) <- transLValue lv
    (_, eT)  <- transExpr e
    matchTwo eT lvT (erf "value of RHS does not match LHS") $
        return ((), TUnit)
transExpr (N.IfEx p b1 mb2) = do
    (_, pT) <- transExpr p
    expectInt pT (erf "can only calculate truthiness of integers") $ do
        (_, t1) <- transExpr b1
        case mb2 of
            Just b2 -> do
                (_, t2) <- transExpr b2
                matchTwo t1 t2 (erf "both branches of an if must produce the same type") $
                    return ((), t1)
            Nothing -> do
                expectUnit t1 (erf "type of if must be unit") $
                    return ((), TUnit)
transExpr (N.WhileEx p bd) = do
    (_, pT) <- transExpr p
    expectInt pT (erf "can only calculate truthiness of integers") $ do
        (_, bdT) <- transExpr bd
        expectUnit bdT (erf "body of while can't return a value") $
            return ((), TUnit)
transExpr (N.ForEx iterator ini ub bd) = do
    (_, iniT) <- transExpr ini
    expectInt iniT (erf "initialiser must be integer") $ do
        (_, ubT) <- transExpr ub
        expectInt ubT (erf "upper bound must be integer") $ do
            addVar iterator TInt $ do
                (_, bdT) <- transExpr bd
                expectUnit bdT (erf "for body must not return anything") $
                    return ((), TUnit)
transExpr N.BreakEx = return ((), TUnit)
transExpr (N.LetEx cs []) = transChunks cs $ return ((), TUnit)
transExpr (N.LetEx cs bd) = transChunks cs (transExpr (N.Exs bd))


transLValue :: (SymbolTable t) => N.LValue -> Analyser t Expty
transLValue (N.IdLV i) = do
    findEnvEntry i (erf "variable not found") $ \envEntry ->
        case envEntry of
            (VarEntry t) -> return ((), t)
            _ -> erf "cannot modify a function"
transLValue (N.RecLV lvalue (N.Id i)) = do
    (_, recordT) <- transLValue lvalue
    expectRecord recordT (erf "cannot get member from a non-record") $ \pairs _ ->
        case lookup i pairs of
            Nothing -> erf "member does not exist in record"
            Just ty -> return ((), ty)

transLValue (N.ArrLV lvalue e) = do
    (_, arrayT) <- transLValue lvalue
    expectArray arrayT (erf "cannot index a non-array") $ \elementT _ -> do
        (_, eT) <- transExpr e
        expectInt eT (erf "array index must be an integer") $
            return ((), elementT)


transChunk :: (SymbolTable t) => N.Chunk -> Analyser t a -> Analyser t a
transChunk (N.TypeChunk []) f = f
transChunk (N.TypeChunk ((N.TypeDecl i t):ts)) f = do
    ty <- transTy t
    let tid = N.idToTyId i
    notFindTy tid (err "type name clashes with existing type" >> f) $
        addType tid ty $
            transChunk (N.TypeChunk ts) f
transChunk (N.FunChunk []) f = f
transChunk (N.FunChunk ((N.Function fid (TyFields params) maybeRetT body) : fs)) f = do
    notFindEnvEntry fid (err "function name clashes with existing name" >> f) $ do
        paramTys <- traverse (\(_, p) -> findTy p (snd <$> erf "parameter type not found") return) params
        let paramsWithTys = map fst params `zip` paramTys
        addVars paramsWithTys $ do
            case maybeRetT of
                Just tid -> do
                    findTy tid (err "cannot find return type" >> f) $ \retT ->
                        addFun fid paramTys retT $ do
                            (_, retT') <- transExpr body
                            matchTwo retT retT' (err "actual return type does not match provided" >> f) $
                                transChunk (N.FunChunk fs) f
                Nothing -> 
                    addFun fid paramTys TUnknown $ do
                        (_, retT') <- transExpr body
                        addFun fid paramTys retT' $
                            transChunk (N.FunChunk fs) f


    -- TODO: might change with mutual recursion
transChunk (N.FunChunk ((N.Primitive fid (TyFields params) maybeRetT) : fs)) f = do
    notFindEnvEntry fid (err "primitive name clashes with existing name" >> f) $ do
        paramTys <- traverse (\(_, p) -> findTy p (snd <$> erf "parameter type not found") return) params
        let paramsWithTys = map fst params `zip` paramTys
        addVars paramsWithTys $ do
            case maybeRetT of
                Just tid -> do
                    findTy tid (err "cannot find return type" >> f) $ \retT ->
                        addFun fid paramTys retT $ transChunk (N.FunChunk fs) f
                Nothing -> err "unknown primitive" >> transChunk (N.FunChunk fs) f
transChunk (N.VarChunk (N.VarDecl i maybeT e)) f = do
    (_, t) <- transExpr e
    notFindEnvEntry i (err "variable name clashes with existing name" >> f) $ do
        case maybeT of
            Just tyId -> do
                findTy tyId (err "return type does not exist" >> f) $ \t' -> do
                    matchTwo t t' (err "inferred type does not match actual type" >> f) $
                        addVar i t f
            Nothing -> addVar i t f


transChunk (N.ImportChunk _) f = err "you should not be importing..." >> f

transChunks :: (SymbolTable t) => [N.Chunk] -> Analyser t a -> Analyser t a
transChunks cs f = foldr transChunk f cs

transOp :: N.Op -> Expty -> Expty -> Analyser t Expty
transOp N.AddOp (_, lt) (_, rt) = do
    expectInt lt (erf "cannot add non-integers") $
        expectInt rt (erf "cannot add non-integers") $
            return ((), TInt)
transOp N.SubOp (_, lt) (_, rt) = do
    expectInt lt (erf "cannot subtract non-integers") $
        expectInt rt (erf "cannot subtract non-integers") $
            return ((), TInt)
transOp N.MultOp (_, lt) (_, rt) = do
    expectInt lt (erf "cannot multiply non-integers") $
        expectInt rt (erf "cannot multiply non-integers") $
            return ((), TInt)
transOp N.DivOp (_, lt) (_, rt) = do
    expectInt lt (erf "cannot divide non-integers") $
        expectInt rt (erf "cannot divide non-integers") $
            return ((), TInt)
transOp N.EqOp (_, lt) (_, rt) = do
    matchTwo lt rt (erf "cannot check for equality on different types") $
        return ((), TInt)
transOp N.NeqOp (_, lt) (_, rt) = do
    matchTwo lt rt (erf "cannot check for inequality on different types") $
        return ((), TInt)
transOp N.LtOp (_, lt) (_, rt) = do
    expectInt lt (erf "cannot compare non-integers") $
        expectInt rt (erf "cannot compare non-integers") $
            return ((), TInt)
transOp N.LeOp (_, lt) (_, rt) = do
    expectInt lt (erf "cannot compare non-integers") $
        expectInt rt (erf "cannot compare non-integers") $
            return ((), TInt)
transOp N.GtOp (_, lt) (_, rt) = do
    expectInt lt (erf "cannot compare non-integers") $
        expectInt rt (erf "cannot compare non-integers") $
            return ((), TInt)
transOp N.GeOp (_, lt) (_, rt) = do
    expectInt lt (erf "cannot compare non-integers") $
        expectInt rt (erf "cannot compare non-integers") $
            return ((), TInt)
transOp N.AndOp (_, lt) (_, rt) = do
    expectInt lt (erf "cannot AND non-integers") $
        expectInt rt (erf "cannot AND non-integers") $
            return ((), TInt)
transOp N.OrOp (_, lt) (_, rt) = do
    expectInt lt (erf "cannot OR non-integers") $
        expectInt rt (erf "cannot OR non-integers") $
            return ((), TInt)

analyse :: (SymbolTable t) => N.Program -> Analyser t ()
analyse (N.ExprProg e)       = void (transExpr e)
analyse (N.ChunkProg [])     = return ()
analyse (N.ChunkProg (c:cs)) = transChunk c (analyse (N.ChunkProg cs))

runSemant :: N.Program -> [String]
-- runSemant p = evalStateT (analyse p) initialData
--     where initialData = Data { currentEnv   = eVenv :: MST EnvEntry
--                              , currentTyEnv = eTenv :: MST Ty
--                              , nextId       = 0                     }
runSemant p = w
    where (_, _, w) = runRWS (analyse p) (Env (eVenv :: MST EnvEntry) (eTenv :: MST Ty)) (Data { nextId = 0 })