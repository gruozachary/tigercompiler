module Semantics.Analyser
    ( Ty(..), EnvEntry(..), Exp, Expty, Data(..), Error, Analyser
    , getNextId, matchTy, expectInt, expectString, expectRecord, expectArray
    , expectNil, expectUnit, expectName
    ) where
import Control.Monad.State (StateT, gets, modify, MonadTrans (lift))

data Ty
    = TInt
    | TString
    | TRecord [(String, Ty)] Int
    | TArray Ty Int
    | TNil
    | TUnit
    | TName String (Maybe Ty) deriving Eq

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

matchTy :: Error -> Ty -> Ty -> Analyser t ()
matchTy e t0 t1
    | t0 == t1  = return ()
    | otherwise = lift (Left e)

expectInt :: Error -> Ty -> Analyser t ()
expectString :: Error -> Ty -> Analyser t ()
expectRecord :: Error -> Ty -> Analyser t ([(String, Ty)], Int)
expectArray :: Error -> Ty -> Analyser t (Ty, Int)
expectNil :: Error -> Ty -> Analyser t ()
expectUnit :: Error -> Ty -> Analyser t ()
expectName :: Error -> Ty -> Analyser t (String, Maybe Ty)

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
