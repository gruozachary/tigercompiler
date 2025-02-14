module Semantics.Analyser
    ( Ty(..), EnvEntry(..), Exp, Expty, Data(..), Error, Analyser
    , getNextId, matchTwo, expectInt, expectString, expectRecord, expectArray
    , expectNil, expectUnit, expectName, expectFun, expectJust
    , findEnvEntry, findTy, addType, addFun, addVar
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

errFail :: Error -> Analyser t Expty
errFail e = err e >> pure ((), TUnknown)

getNextId :: Analyser t Int
getNextId = do
    i <- gets nextId
    modify (\s -> s { nextId = i + 1 })
    pure i

matchTwo :: (Eq a) => Error -> a -> a -> Analyser t Expty -> Analyser t Expty
matchTwo e t0 t1 f
    | t0 == t1  = f
    | otherwise = errFail e

expectInt :: Error -> Ty -> Analyser t Expty -> Analyser t Expty
expectInt _ TInt f = f
expectInt e _ _ = errFail e

expectString :: Error -> Ty -> Analyser t Expty -> Analyser t Expty
expectString _ TString f = f
expectString e _ _ = errFail e

expectRecord :: Error -> Ty -> ([(String, Ty)] -> Int -> Analyser t Expty) -> Analyser t Expty
expectRecord _ (TRecord ps i) f = f ps i
expectRecord e _ _ = errFail e

expectArray :: Error -> Ty -> (Ty -> Int -> Analyser t Expty) -> Analyser t Expty
expectArray _ (TArray t i) f = f t i
expectArray e _ _ = errFail e

expectNil :: Error -> Ty -> Analyser t Expty -> Analyser t Expty
expectNil _ TNil f = f
expectNil e _ _ = errFail e

expectUnit :: Error -> Ty -> Analyser t Expty -> Analyser t Expty
expectUnit _ TUnit f = f
expectUnit e _ _ = errFail e

expectName :: Error -> Ty -> (String -> Maybe Ty -> Analyser t Expty) -> Analyser t Expty
expectName _ (TName s t) f = f s t
expectName e _ _ = errFail e

expectFun :: Error -> EnvEntry -> ([Ty] -> Ty -> Analyser t Expty) -> Analyser t Expty
expectFun _ (FunEntry ts t) f = f ts t
expectFun e _ _ = errFail e

expectJust :: Error -> Maybe a -> (a -> Analyser t Expty) -> Analyser t Expty
expectJust _ (Just x) f = f x
expectJust e _ _ = errFail e

findEnvEntry :: (SymbolTable t) => Error -> N.Id -> (EnvEntry -> Analyser t Expty) -> Analyser t Expty
findEnvEntry e (N.Id i) f = do
    Env venv _ <- ask
    case look venv i of
        Just x -> f x
        Nothing -> errFail e

findTy :: (SymbolTable t) => Error -> N.TyId -> (Ty -> Analyser t Expty) -> Analyser t Expty
findTy e (N.TyId i) f = do
    Env _ tenv <- ask
    case look tenv i of
        Just x -> f x
        Nothing -> errFail e

addType :: (SymbolTable t) => N.TyId -> Ty -> Analyser t a -> Analyser t a
addType (N.TyId i) t = local (\(Env venv tenv) -> Env venv (insert tenv i t))

addFun :: (SymbolTable t) => N.Id -> [Ty] -> Ty -> Analyser t a -> Analyser t a
addFun (N.Id i) ps r = local (\(Env venv tenv) -> Env (insert venv i (FunEntry ps r)) tenv)

addVar :: (SymbolTable t) => N.Id -> Ty -> Analyser t a -> Analyser t a
addVar (N.Id i) t = local (\(Env venv tenv) -> Env (insert venv i (VarEntry t)) tenv)