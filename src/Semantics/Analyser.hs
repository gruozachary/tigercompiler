module Semantics.Analyser
    ( Ty(..), EnvEntry(..), Exp, Expty, Data(..), Error, Analyser
    , getNextId, matchTwo, expectInt, expectString, expectRecord, expectArray
    , expectNil, expectUnit, expectName, expectFun, expectJust
    , findEnvEntry, findTy, addType, addFun, addVar
    ) where
import Control.Monad.State (StateT, gets, modify, lift, get, put)
import qualified Parsing.Nodes as N
import Semantics.SymbolTable

data Ty
    = TInt
    | TString
    | TRecord [(String, Ty)] Int
    | TArray Ty Int
    | TNil
    | TUnit
    | TName String (Maybe Ty)
    deriving (Ord, Eq)

-- data type for entries into the venv
data EnvEntry
    = VarEntry Ty
    | FunEntry [Ty] Ty

type Exp = () -- TODO add implementation (intermediate code generation stage)
type Expty = (Exp, Ty)

data Data t = Data
    { currentEnv :: t EnvEntry
    , currentTyEnv :: t Ty
    , nextId :: Int
    }

type Error = String
type Analyser t a = StateT (Data t) (Either Error) a

getNextId :: Analyser t Int
getNextId = do
    i <- gets nextId
    modify (\s -> s { nextId = i + 1 })
    return i

matchTwo :: (Eq a) => Error -> a -> a -> Analyser t ()
matchTwo e t0 t1
    | t0 == t1  = return ()
    | otherwise = lift (Left e)

expectInt :: Error -> Ty -> Analyser t ()
expectString :: Error -> Ty -> Analyser t ()
expectRecord :: Error -> Ty -> Analyser t ([(String, Ty)], Int)
expectArray :: Error -> Ty -> Analyser t (Ty, Int)
expectNil :: Error -> Ty -> Analyser t ()
expectUnit :: Error -> Ty -> Analyser t ()
expectName :: Error -> Ty -> Analyser t (String, Maybe Ty)

expectFun :: Error -> EnvEntry -> Analyser t ([Ty], Ty)

expectInt _ TInt = return ()
expectInt e _ = lift (Left e)
expectString _ TString = return ()
expectString e _ = lift (Left e)
expectRecord _ (TRecord x y) = return (x, y)
expectRecord e _ = lift (Left e)
expectArray _ (TArray x y) = return (x, y)
expectArray e _ = lift (Left e)
expectNil _ TNil = return ()
expectNil e _ = lift (Left e)
expectUnit _ TUnit = return ()
expectUnit e _ = lift (Left e)
expectName _ (TName x y) = return (x, y)
expectName e _ = lift (Left e)

expectFun _ (FunEntry x y) = return (x, y)
expectFun e _ = lift (Left e)

expectJust :: Error -> Maybe a -> Analyser t a
expectJust e Nothing = lift (Left e)
expectJust _ (Just x) = return x

findEnvEntry :: (SymbolTable t) => Error -> N.Id -> Analyser t EnvEntry
findEnvEntry  e (N.Id i) = do
    env <- gets currentEnv
    expectJust e (look env i)

findTy :: (SymbolTable t) => Error -> N.TyId -> Analyser t Ty
findTy e (N.TyId i) = do
    tenv <- gets currentTyEnv
    expectJust e (look tenv i)

addType :: (SymbolTable t) => N.TyId -> Ty -> Analyser t ()
addType (N.TyId i) t = do
    state <- get
    put (state { currentTyEnv = insert (currentTyEnv state) i t })

addFun :: (SymbolTable t) => N.Id -> [Ty] -> Ty -> Analyser t ()
addFun (N.Id i) as r = do
    state <- get
    put (state { currentEnv = insert (currentEnv state) i (FunEntry as r) })

addVar :: (SymbolTable t) => N.Id -> Ty -> Analyser t ()
addVar (N.Id i) t = do
    state <- get
    put (state { currentEnv = insert (currentEnv state) i (VarEntry t) })