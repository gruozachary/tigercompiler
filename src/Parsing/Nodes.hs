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
    , idToTyId, tyIdToId ) where

newtype Id = Id String deriving (Show, Eq)
newtype TyId = TyId String deriving (Show, Eq)

idToTyId :: Id -> TyId
idToTyId (Id x) = TyId x

tyIdToId :: TyId -> Id
tyIdToId (TyId x) = Id x

data Program = ExprProg Expr | ChunkProg [Chunk] deriving (Show, Eq)

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
    deriving (Show, Eq)

data LValue
    = IdLV Id
    | RecLV LValue Id
    | ArrLV LValue Expr
    deriving (Show, Eq)

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
    deriving (Show, Eq)

data Chunk
    = TypeChunk [TypeDecl]
    | FunChunk [FunDecl]
    | VarChunk VarDecl
    | ImportChunk String
    deriving (Show, Eq)

data FunDecl
    = Function Id TyFields (Maybe TyId) Expr
    | Primitive Id TyFields (Maybe TyId)
    deriving (Show, Eq)
data TypeDecl = TypeDecl Id Type deriving (Show, Eq)
data VarDecl = VarDecl Id (Maybe TyId) Expr deriving (Show, Eq)

data Type
    = IdTy TyId
    | RecordTy TyFields
    | ArrayTy TyId
    deriving (Show, Eq)

newtype TyFields = TyFields [(Id, TyId)] deriving (Show, Eq)