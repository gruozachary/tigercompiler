module Parsing.AstNode (
    Id, 
    Node(..),
    Program(..), 
    Expr(..), 
    LValue(..), 
    Op(..), 
    Chunk(..),
    FunDecl(..), 
    TypeDecl(..), 
    VarDecl(..), 
    Type(..), 
    TyFields(..), 
    TyId(..)) where

type Id = String

data Node
    = ProgramNode Program
    | ExprsNode [Expr]
    | ExprNode Expr
    | LValueNode LValue
    | OpNode Op
    | ChunksNode [Chunk]
    | ChunkNode Chunk
    | VarDeclNode VarDecl
    | TypeDeclNode TypeDecl
    | FunDeclNode FunDecl
    | TypeNode Type
    | TyFieldsNode TyFields
    | TyIdNode TyId

data Program = ExprProg Expr | ChunkProg [Chunk]

data Expr
    = NilEx
    | IntEx Int
    | StrEx String
    | ArrayEx TyId Expr Expr
    | RecordEx Id [(TyId, Expr)]
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
newtype TyId = TyId String