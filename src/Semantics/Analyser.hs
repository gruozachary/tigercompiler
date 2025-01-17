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

    -- * might need some sort of unique identifier

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


transTy :: (SymbolTable t) => N.Type -> Analyser t Ty
transTy = undefined

transExpr :: (SymbolTable t) => N.Expr -> Analyser t Expty
transExpr N.NilEx = return ((), TNil)
transExpr (N.IntEx _) = return ((), TInt)
transExpr (N.StrEx _) = return ((), TString)
transExpr (N.ArrayEx tid size element) = do
    arrayT <- transTy (N.IdTy tid)
    (et, i) <- case arrayT of
        (TArray t i) -> return (t, i)
        _ -> lift (Left "type of array is not real")
    case size of
        N.IntEx _ -> return ()
        _ -> lift (Left "size of array is not an integer literal")
    elementT <- transExpr element
    if snd elementT == et
        then return ((), TArray et i) 
        else lift (Left "array type does not match initial elements")
transExpr (N.RecordEx tid entries) = undefined
    -- t <- transTy (N.IdTy tid)
transExpr (N.LValEx _) = undefined
transExpr (N.FunCallEx _ _ ) = undefined
transExpr (N.NegEx _ ) = undefined
transExpr (N.OpEx _ _ _) = undefined
transExpr (N.Exs _) = undefined
transExpr (N.AssignEx _ _ ) = undefined
transExpr (N.IfEx _ _ _) = undefined
transExpr (N.WhileEx _ _) = undefined
transExpr (N.ForEx _ _ _ _) = undefined
transExpr (N.BreakEx) = undefined
transExpr (N.LetEx _ _) = undefined

transChunk :: (SymbolTable t) => N.Chunk -> Analyser t ()
transChunk = undefined

analyse :: (SymbolTable t) => N.Program -> Analyser t ()
analyse (N.ExprProg e)       = void (transExpr e)
analyse (N.ChunkProg [])     = return ()
analyse (N.ChunkProg (c:cs)) = transChunk c >> analyse (N.ChunkProg cs)
