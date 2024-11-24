{
module Parsing.Parser where

import qualified Lexing.Lexer as L
import Parsing.Nodes
}

%name parse
%tokentype { L.Token }
%error { parseError }
%monad { L.Alex } { >>= } { return }
%lexer { lexer } { L.EOF }

-- precedences
%left '|'
%left '&'
%nonassoc '>=' '<=' '=' '<>' '<' '>'
%left '+' '-'
%left '*' '/'

%token
    array { L.Array } 
    if { L.If }
    then { L.Then } 
    else { L.Else } 
    while { L.While } 
    for { L.For } 
    to { L.To } 
    do { L.Do } 
    let { L.Let } 
    in { L.In } 
    end { L.End } 
    of { L.Of } 
    break { L.Break } 
    nil { L.Nil } 
    function { L.Function } 
    var { L.Var } 
    type { L.Type } 
    imprt { L.Import } 
    primitive { L.Primitive } 
    -- Symbols
    ',' { L.Comma } 
    ':' { L.Colon } 
    ';' { L.Semicolon } 
    '(' { L.LeftParen } 
    ')' { L.RightParen } 
    '[' { L.LeftSquare } 
    ']' { L.RightSquare } 
    '{' { L.LeftBrace } 
    '}' { L.RightBrace } 
    '.' { L.Period } 
    '+' { L.Plus } 
    '-' { L.Minus } 
    '*' { L.Star } 
    '/' { L.Slash } 
    '=' { L.Equal } 
    '<>' { L.DoubleAngle } 
    '<' { L.LeftAngle } 
    '>' { L.RightAngle } 
    '<=' { L.LeftAngleEqual } 
    '>=' { L.RightAngleEqual } 
    '&' { L.Ampersand } 
    '|' { L.Pipe } 
    ':=' { L.ColonEqual }
    -- Non-keyword
    idT { L.Id $$ }
    stringLiteral { L.StringLiteral $$ }
    numberLiteral { L.NumberLiteral $$ }
    

%%
program :: { Program }
    : exp { ExprProg $1 }
    | chunks { ChunkProg $1 }

exps :: { [Expr] }
    : exp moreExps { $1 : $2 }
    | {-empty-} { [] }
moreExps :: { [Expr] }
    : ';' exp moreExps { $2 : $3 }
    | {-empty-} { [] }
exp :: { Expr }
    : nil { NilEx }
    | numberLiteral { IntEx $1 }
    | stringLiteral { StrEx $1 }
-- array and record creations
    | typeId '[' exp ']' of exp { ArrayEx $1 $3 $6 }
    | typeId '{' '}' { RecordEx $1 [] }
    | typeId '{' id '=' exp recordSubs '}' { RecordEx $1 (($3, $5) : $6) }
-- variables, field, elements of an array
    | lvalue { LValEx $1 }
-- function call
    | id '(' ')' { FunCallEx $1 [] }
    | id '(' args ')' { FunCallEx $1 $3 }
-- operations
    | '-' exp { NegEx $2 }
    | exp op exp { OpEx $1 $2 $3 }
    | '(' exps ')' { Exs $2 }
-- assignment
    | lvalue ':=' exp { AssignEx $1 $3 }
-- control structures
    | if exp then exp else exp { IfEx $2 $4 (Just $6) }
    | if exp then exp { IfEx $2 $4 Nothing }
    | while exp do exp { WhileEx $2 $4 }
    | for id ':=' exp to exp do exp { ForEx $2 $4 $6 $8 }
    | break { BreakEx }
    | let chunks in exps end { LetEx $2 $4 }

-- helper rules
args :: { [Expr] }
    : exp moreArgs { $1 : $2 }
moreArgs :: { [Expr] }
    : ',' exp moreArgs { $2 : $3 }
    | {-empty-} { [] }

recordSubs :: { [(Id, Expr)] }
    : ',' id '=' exp recordSubs { ($2, $4) : $5 }
    | {-empty-} { [] }

-- LHS values
lvalue :: { LValue }
    : id { IdLV $1 }
    | lvalue '.' id { RecLV $1 $3 }
    | lvalue '[' exp ']' { ArrLV $1 $3 }

-- operators
op :: { Op }
    : '+' { AddOp }
    | '-' { SubOp }
    | '*' { MultOp }
    | '/' { DivOp }
    | '=' { EqOp }
    | '<>' { NeqOp }
    | '>' { GtOp }
    | '<' { LtOp }
    | '>=' { GeOp }
    | '<=' { LeOp }
    | '&' { AndOp }
    | '|' { OrOp }


-- chunks of declarations
chunks :: { [Chunk] }
    : chunk chunks { $1 : $2 }
    | {-empty-} { [] }
chunk :: { Chunk }
    : tydecs { TypeChunk $1 }
    | fundecs { FunChunk $1 }
    | vardec { VarChunk $1 }
    | imprt stringLiteral { ImportChunk $2 }

fundecs :: { [FunDecl] }
    : fundec fundecs { $1 : $2 }
    | {-empty-} { [] }

tydecs :: { [TypeDecl] }
    : tydec tydecs { $1 : $2 }
    | {-empty-} { [] }

-- variable declaration
vardec :: { VarDecl }
    : var id ':=' exp { VarDecl $2 Nothing $4 }
    | var id ':' typeId ':=' exp { VarDecl $2 (Just $4) $6 }

-- type declaration
tydec :: { TypeDecl }
    : type id '=' ty { TypeDecl $2 $4 }

-- function declaration
fundec :: { FunDecl }
    : function id '(' tyFields ')' '=' exp { Function $2 $4 Nothing $7 }
    | function id '(' tyFields ')' ':' typeId '=' exp { Function $2 $4 (Just $7) $9 }
    | primitive id '(' tyFields ')' { Primitive $2 $4 Nothing } 
    | primitive id '(' tyFields ')' ':' typeId { Primitive $2 $4 (Just $7) }

-- types
ty :: { Type }
    : typeId  { IdTy $1 }
    | '{' tyFields '}' { RecordTy $2 } 
    | array of typeId { ArrayTy $3 }

tyFields :: { TyFields }
    : id ':' typeId moreTyFields { TyFields (($1, $3) : $4)}
moreTyFields :: { [(Id, TyId)] }
    : ',' id ':' typeId moreTyFields { ($2, $4) : $5 } 
    | {-empty-} { [] }

typeId :: { TyId }
    : id { idToTyId $1 }

id :: { Id }
    : idT { Id $1 }

{
parseError _ = do
    ((L.AlexPn _ line column), _, _, _) <- L.alexGetInput
    L.alexError ("parse error at line " ++ (show line) ++ ", column " ++ (show column))

lexer :: (L.Token -> L.Alex a) -> L.Alex a
lexer f = do
    m <- L.alexMonadScan
    case m of
        Nothing -> lexer f
        (Just x) -> f x
}