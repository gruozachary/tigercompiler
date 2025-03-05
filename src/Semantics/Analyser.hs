module Semantics.Analyser
    ( Ty(..), EnvEntry(..), Exp, Expty, Data(..), Error, Analyser, Env(..)
    , getNextId, matchTwo, expectInt, expectString, expectRecord, expectArray
    , expectNil, expectUnit, expectName, expectFun
    , findEnvEntry, findTy, notFindEnvEntry, notFindTy
    , addType, addFun, addVar, addVars
    , err, erf
    ) where
import qualified Parsing.Nodes as N
import Semantics.SymbolTable
import Control.Monad.RWS (RWS, gets, modify, tell, ask, local)

data Ty
    = TInt
    | TString
    | TRecord [(String, Ty)] Int
    | TArray Ty Int
    | TNil
    | TUnit
    | TUnknown
    | TName String (Maybe Ty)
    deriving (Ord, Eq)

-- data type for entries into the venv
data EnvEntry
    = VarEntry Ty
    | FunEntry [Ty] Ty

type Exp = () -- TODO add implementation (intermediate code generation stage)
type Expty = (Exp, Ty)

data Env t = Env (t EnvEntry) (t Ty)

data Data = Data
    { nextId :: Int
    }

type Error = String
type Analyser t a = RWS (Env t) [Error] Data a -- TODO: difference list

err :: Error -> Analyser t ()
err e = tell [e]

erf :: Error -> Analyser t Expty
erf e = err e >> pure ((), TUnknown)

getNextId :: Analyser t Int
getNextId = do
    i <- gets nextId
    modify (\s -> s { nextId = i + 1 })
    pure i

matchTwo :: (Eq a) => a -> a -> Analyser t b -> Analyser t b -> Analyser t b
matchTwo t0 t1 fl su
    | t0 == t1  = su
    | otherwise = fl

expectInt :: Ty -> Analyser t a -> Analyser t a -> Analyser t a
expectInt TInt _  su = su
expectInt _    fl _  = fl

expectString :: Ty -> Analyser t a -> Analyser t a -> Analyser t a
expectString TString _  su = su
expectString _       fl _  = fl

expectRecord :: Ty -> Analyser t a -> ([(String, Ty)] -> Int -> Analyser t a) -> Analyser t a
expectRecord (TRecord ps i) _  su = su ps i
expectRecord _              fl _  = fl

expectArray :: Ty -> Analyser t a -> (Ty -> Int -> Analyser t a) -> Analyser t a
expectArray (TArray t i) _  su = su t i
expectArray _            fl _  = fl

expectNil :: Ty -> Analyser t a -> Analyser t a -> Analyser t a
expectNil TNil _  su = su
expectNil _    fl _  = fl

expectUnit :: Ty -> Analyser t a -> Analyser t a -> Analyser t a
expectUnit TUnit _  su = su
expectUnit _     fl _  = fl

expectName :: Ty -> Analyser t a -> (String -> Maybe Ty -> Analyser t a) -> Analyser t a
expectName (TName s t) _  su = su s t
expectName _           fl _  = fl

expectFun :: EnvEntry -> Analyser t a -> ([Ty] -> Ty -> Analyser t a) -> Analyser t a
expectFun (FunEntry ts t) _  su = su ts t
expectFun _               fl _  = fl

findEnvEntry :: (SymbolTable t) => N.Id -> Analyser t a -> (EnvEntry -> Analyser t a) -> Analyser t a
findEnvEntry (N.Id i) fl su = do
    Env venv _ <- ask
    maybe fl su (look venv i)

findTy :: (SymbolTable t) => N.TyId -> Analyser t a -> (Ty -> Analyser t a) -> Analyser t a
findTy (N.TyId i) fl su = do
    Env _ tenv <- ask
    maybe fl su (look tenv i)

notFindEnvEntry :: (SymbolTable t) => N.Id -> Analyser t a -> Analyser t a -> Analyser t a
notFindEnvEntry (N.Id i) isFound notFound = do
    Env venv _ <- ask
    let entry = look venv i
    case entry of
        Just _ -> isFound
        Nothing -> notFound

notFindTy :: (SymbolTable t) => N.TyId -> Analyser t a -> Analyser t a -> Analyser t a
notFindTy (N.TyId i) isFound notFound = do
    Env _ tenv <- ask
    let entry = look tenv i
    case entry of
        Just _ -> isFound
        Nothing -> notFound

addType :: (SymbolTable t) => N.TyId -> Ty -> Analyser t a -> Analyser t a
addType (N.TyId i) t = local (\(Env venv tenv) -> Env venv (insert tenv i t))

addFun :: (SymbolTable t) => N.Id -> [Ty] -> Ty -> Analyser t a -> Analyser t a
addFun (N.Id i) ps r = local (\(Env venv tenv) -> Env (insert venv i (FunEntry ps r)) tenv)

addVar :: (SymbolTable t) => N.Id -> Ty -> Analyser t a -> Analyser t a
addVar (N.Id i) t = local (\(Env venv tenv) -> Env (insert venv i (VarEntry t)) tenv)

addVars :: (SymbolTable t) => [(N.Id, Ty)] -> Analyser t a -> Analyser t a
addVars = foldr (\(i, t) k -> k . addVar i t) id