module Semantics.Analyser
    (
    ) where

import Semantics.SymbolTable (SymbolTable (new, fromList))
import qualified Parsing.Nodes as N
import Control.Monad.State (StateT, lift, gets, modify)
import Control.Monad (void)

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

eVenv :: (SymbolTable t) => t EnvEntry
eTenv :: (SymbolTable t) => t Ty
eVenv = new
eTenv = fromList [("string", TString), ("int", TInt)]
-- empty venv and tenv

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

transTy :: (SymbolTable t) => N.Type -> Analyser t Ty
transTy = undefined

transExpr :: (SymbolTable t) => N.Expr -> Analyser t Expty
transExpr N.NilEx = return ((), TNil)
transExpr (N.IntEx _) = return ((), TInt)
transExpr (N.StrEx _) = return ((), TString)
transExpr (N.ArrayEx tid size element) = do
    t <- transTy (N.IdTy tid)
    (et, _) <- expectArray "type of array is not defined" t
    case size of
        N.IntEx _ -> return ()
        _ -> lift (Left "size of array is not an integer literal")
    (_, et') <- transExpr element
    matchTy "array type does not match element initialiser" et et'
    return ((), t)
transExpr (N.RecordEx tid entries) = undefined
transExpr (N.LValEx _) = undefined 
transExpr (N.FunCallEx _ _ ) = undefined
transExpr (N.NegEx _ ) = undefined
transExpr (N.OpEx _ _ _) = undefined
transExpr (N.Exs _) = undefined
transExpr (N.AssignEx _ _ ) = undefined
transExpr (N.IfEx _ _ _) = undefined
transExpr (N.WhileEx _ _) = undefined
transExpr (N.ForEx _ _ _ _) = undefined
transExpr (N.BreakEx) = return ((), TUnit)
transExpr (N.LetEx _ _) = undefined

transChunk :: (SymbolTable t) => N.Chunk -> Analyser t ()
transChunk = undefined

analyse :: (SymbolTable t) => N.Program -> Analyser t ()
analyse (N.ExprProg e)       = void (transExpr e)
analyse (N.ChunkProg [])     = return ()
analyse (N.ChunkProg (c:cs)) = transChunk c >> analyse (N.ChunkProg cs)
