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

expectInt :: Error -> Analyser t Expty -> Ty -> Analyser t Expty
expectInt _ f TInt = f
expectInt e f _ = errFail e

expectString :: Error -> Analyser t Expty -> Ty -> Analyser t Expty
expectString _ f TString = f
expectString e _ _ = errFail e

expectRecord :: Error -> ([(String, Ty)] -> Int -> Analyser t Expty) -> Ty -> Analyser t Expty
expectRecord _ f (TRecord ps i) = f ps i
expectRecord e _ _ = errFail e

expectArray :: Error -> (Ty -> Int -> Analyser t Expty) -> Ty -> Analyser t Expty
expectArray _ f (TArray t i) = f t i
expectArray e _ _ = errFail e

expectNil :: Error -> Analyser t Expty -> Ty -> Analyser t Expty
expectNil _ f TNil = f
expectNil e _ _ = errFail e

expectUnit :: Error -> Analyser t Expty -> Ty -> Analyser t Expty
expectUnit _ f TUnit = f
expectUnit e _ _ = errFail e

expectName :: Error -> (String -> Maybe Ty -> Analyser t Expty) -> Ty -> Analyser t Expty
expectName _ f (TName s t) = f s t
expectName e _ _ = errFail e

expectFun :: Error -> ([Ty] -> Ty -> Analyser t Expty) -> EnvEntry -> Analyser t Expty
expectFun _ f (FunEntry ts t) = f ts t
expectFun e _ _ = errFail e

expectJust :: Error -> (a -> Analyser t Expty) -> Maybe a -> Analyser t Expty
expectJust _ f (Just x) = f x
expectJust e _ _ = errFail e

findEnvEntry :: (SymbolTable t) => Error -> (EnvEntry -> Analyser t Expty) -> N.Id -> Analyser t Expty
findEnvEntry e f (N.Id i) = do
    Env venv _ <- ask
    case look venv i of
        Just x -> f x
        Nothing -> errFail e

findTy :: (SymbolTable t) => Error -> (Ty -> Analyser t Expty) -> N.TyId -> Analyser t Expty
findTy e f (N.TyId i) = do
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