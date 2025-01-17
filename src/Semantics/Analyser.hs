module Semantics.Analyser
    (
    ) where

import Semantics.SymbolTable (SymbolTable (new))
import qualified Parsing.Nodes as N
import Control.Monad.State (State)
import Control.Monad (void)

data Ty
    = TInt
    | TString
    | TRecord [(String, Ty)] -- *
    | TArray Ty              -- *
    | Nil
    | Unit
    | Name String (Maybe Ty) -- *

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
eTenv = new
-- empty venv and tenv

data Data t = Data
    { currentEnv :: t EnvEntry
    , currentTyEnv :: t Ty
    }

type Analyser t a = State (Data t) a

transExpr :: (SymbolTable t) => N.Expr -> Analyser t Expty
transExpr = undefined

transChunk :: (SymbolTable t) => N.Chunk -> Analyser t ()
transChunk = undefined

analyse :: (SymbolTable t) => N.Program -> Analyser t ()
analyse (N.ExprProg e)       = void (transExpr e)
analyse (N.ChunkProg [])     = return ()
analyse (N.ChunkProg (c:cs)) = transChunk c >> analyse (N.ChunkProg cs)
