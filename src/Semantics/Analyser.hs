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
-- can't derive Eq because Nil type is a valid record

instance Eq Ty where
    TRecord _ _ == TNil = True
    TNil == TRecord _ _ = True -- should this case even be possible?
    TInt == TInt = True
    TString == TString = True
    TRecord atts1 n1 == TRecord atts2 n2 = atts1 == atts2 && n1 == n2
    TArray t1 n1 == TArray t2 n2 = t1 == t2 && n1 == n2
    TNil == TNil = True
    TUnit == TUnit = True
    TUnknown == TUnknown = True
    TName s1 mt1 == TName s2 mt2 = s1 == s2 && mt1 == mt2
    _ == _ = False

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
notFindEnvEntry i isFound notFound = findEnvEntry i notFound (const isFound)

notFindTy :: (SymbolTable t) => N.TyId -> Analyser t a -> Analyser t a -> Analyser t a
notFindTy i isFound notFound = findTy i notFound (const isFound)

addType :: (SymbolTable t) => N.TyId -> Ty -> Analyser t a -> Analyser t a
addType (N.TyId i) t = local (\(Env venv tenv) -> Env venv (insert tenv i t))

addFun :: (SymbolTable t) => N.Id -> [Ty] -> Ty -> Analyser t a -> Analyser t a
addFun (N.Id i) ps r = local (\(Env venv tenv) -> Env (insert venv i (FunEntry ps r)) tenv)

addVar :: (SymbolTable t) => N.Id -> Ty -> Analyser t a -> Analyser t a
addVar (N.Id i) t = local (\(Env venv tenv) -> Env (insert venv i (VarEntry t)) tenv)

addVars :: (SymbolTable t) => [(N.Id, Ty)] -> Analyser t a -> Analyser t a
addVars = foldr (\(i, t) k -> k . addVar i t) id