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

newtype Id = Id String
newtype TyId = TyId String

idToTyId :: Id -> TyId
idToTyId (Id x) = TyId x

data Program = ExprProg Expr | ChunkProg [Chunk]

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

data LValue
    = IdLV Id
    | RecLV LValue Id
    | ArrLV LValue Expr

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

data Chunk
    = TypeChunk [TypeDecl]
    | FunChunk [FunDecl]
    | VarChunk VarDecl
    | ImportChunk String

data FunDecl
    = Function Id TyFields (Maybe TyId) Expr
    | Primitive Id TyFields (Maybe TyId)
data TypeDecl = TypeDecl Id Type
data VarDecl = VarDecl Id (Maybe TyId) Expr

data Type
    = IdTy TyId
    | RecordTy TyFields
    | ArrayTy TyId

newtype TyFields = TyFields [(Id, TyId)]