module Semantics.Semant
    (
    ) where

import Semantics.SymbolTable (SymbolTable (new, fromList))
import qualified Parsing.Nodes as N

import Semantics.Analyser
import Control.Monad (void)
import Control.Monad.Trans (lift)

eVenv :: (SymbolTable t) => t EnvEntry
eTenv :: (SymbolTable t) => t Ty
eVenv = new
eTenv = fromList [("string", TString), ("int", TInt)]
-- empty venv and tenv

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
