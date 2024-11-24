module Parsing.Nodes
    ( Id(..)
    , Program(..)
    , Expr(..)
    , LValue(..)
    , Op(..)
    , Chunk(..)
    , FunDecl(..)
    , TypeDecl(..)
    , VarDecl(..)
    , Type(..)
    , TyFields(..)
    , TyId(..)
    , idToTyId ) where

newtype Id = Id String deriving Show
newtype TyId = TyId String deriving Show

idToTyId :: Id -> TyId
idToTyId (Id x) = TyId x

data Program = ExprProg Expr | ChunkProg [Chunk] deriving Show

data Expr
    = NilEx
    | IntEx Int
    | StrEx String
    | ArrayEx TyId Expr Expr
    | RecordEx TyId [(Id, Expr)]
    | LValEx LValue
    | FunCallEx Id [Expr]
    | NegEx Expr
    | OpEx Expr Op Expr
    | Exs [Expr]
    | AssignEx LValue Expr
    | IfEx Expr Expr (Maybe Expr)
    | WhileEx Expr Expr
    | ForEx Id Expr Expr Expr
    | BreakEx
    | LetEx [Chunk] [Expr]
    deriving Show

data LValue
    = IdLV Id
    | RecLV LValue Id
    | ArrLV LValue Expr
    deriving Show

data Op
    = AddOp
    | SubOp
    | MultOp
    | DivOp
    | EqOp
    | NeqOp
    | LtOp
    | LeOp
    | GtOp
    | GeOp
    | AndOp
    | OrOp
    deriving Show

data Chunk
    = TypeChunk [TypeDecl]
    | FunChunk [FunDecl]
    | VarChunk VarDecl
    | ImportChunk String
    deriving Show

data FunDecl
    = Function Id TyFields (Maybe TyId) Expr
    | Primitive Id TyFields (Maybe TyId)
    deriving Show
data TypeDecl = TypeDecl Id Type deriving Show
data VarDecl = VarDecl Id (Maybe TyId) Expr deriving Show

data Type
    = IdTy TyId
    | RecordTy TyFields
    | ArrayTy TyId
    deriving Show

newtype TyFields = TyFields [(Id, TyId)] deriving Show