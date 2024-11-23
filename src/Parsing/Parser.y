{
module Parsing.Parser where

import Lexing.Lexer
import Parsing.AstNode
}

%name parse
%tokentype { Token }
%error { parseError }
%monad { Alex } { >>= } { return }
%lexer { lexer } { EOF }

-- precedences
%left '|'
%left '&'
%nonassoc '>=' '<=' '=' '<>' '<' '>'
%left '+' '-'
%left '*' '/'

%token
    array { Array } 
    if { If }
    then { Then } 
    else { Else } 
    while { While } 
    for { For } 
    to { To } 
    do { Do } 
    let { Let } 
    in { In } 
    end { End } 
    of { Of } 
    break { Break } 
    nil { Nil } 
    function { Function } 
    var { Var } 
    type { Type } 
    imprt { Import } 
    primitive { Primitive } 
    -- Symbols
    ',' { Comma  } 
    ':' { Colon } 
    ';' { Semicolon } 
    '(' { LeftParen } 
    ')' { RightParen } 
    '[' { LeftSquare } 
    ']' { RightSquare } 
    '{' { LeftBrace } 
    '}' { RightBrace } 
    '.' { Period } 
    '+' { Plus } 
    '-' { Minus } 
    '*' { Star } 
    '/' { Slash } 
    '=' { Equal } 
    '<>' { DoubleAngle } 
    '<' { LeftAngle } 
    '>' { RightAngle } 
    '<=' { LeftAngleEqual } 
    '>=' { RightAngleEqual } 
    '&' { Ampersand } 
    '|' { Pipe } 
    ':=' { ColonEqual }
    -- Non-keyword
    id { Id $$ }
    stringLiteral { StringLiteral $$ }
    numberLiteral { NumberLiteral $$ }
    

%%
program : exp { ProgramNode (ExprProg $1) }
        | chunks { ProgramNode (ChunkProg $1) }

exps : exp moreExps { 
    let ExprNode e = $1 
        ExprsNodes es = $2
    in ExprsNode (e:es)  }
     | {-empty-} { ExprsNode [] }
moreExps : ';' exp moreExps { 
    let ExprNode e = $1 
        ExprsNodes es = $2
    in ExprsNode (e:es) }
         | {-empty-} { ExprsNode [] }
exp : nil { ExprNode NilEx }
    | numberLiteral { ExprNode (IntEx $1) }
    | stringLiteral { ExprNode (StrEx $1) }
-- array and record creations
    | typeId '[' exp ']' of exp { ExprNode (ArrayEx $1 $3 $6) }
    | typeId '{' '}' { Noop }
    | typeId '{' id '=' exp recordSubs '}' { Noop }
-- variables, field, elements of an array
    | lvalue { ExprNode (LValEx) }
-- function call
    | id '(' ')' { Noop }
    | id '(' args ')' { Noop }
-- operations
    | '-' exp { ExprNode (NegEx $2) }
    | exp op exp { let OpNode uOp = $2 in ExprNode (OpEx $1 uOp $3) }
    | '(' exps ')' { let ExprsNode es = $2 in ExprNode (Exs es) }
-- assignment
    | lvalue ':=' exp { ExprNode (AssignEx $1 $3) }
-- control structures
    | if exp then exp else exp { ExprNode (IfEx $2 $4 (Just $6)) }
    | if exp then exp { ExprNode (IfEx $2 $4 Nothing) }
    | while exp do exp { ExprNode (WhileEx $2 $4) }
    | for id ':=' exp to exp do exp { ExprNode (ForEx $2 $4 $6 $8) }
    | break { ExprNode BreakEx }
    | let chunks in exps end { 
        let ChunksNode cs = $2 
            ExprsNode es = $4 
        in ExprNode (LetEx cs es) }

       

args : exp moreArgs { Noop }

moreArgs : ',' exp moreArgs { Noop }
         | {-empty-} { Noop }

recordSubs : ',' id '=' exp recordSubs { Noop }
           | {-empty-} { Noop }

lvalue : id { LValueNode (IdLV $1) }
       | lvalue '.' id { LValueNode (RecLV $1 $3) }
       | lvalue '[' exp ']' { LValueNode (ArrLV $1 $3) }

op : '+' { OpNode (AddOp) }
   | '-' { OpNode (SubOp) }
   | '*' { OpNode (MultOp) }
   | '/' { OpNode (DivOp) }
   | '=' { OpNode (EqOp) }
   | '<>' { OpNode (NeqOp) }
   | '>' { OpNode (LtOp) }
   | '<' { OpNode (GtOp) }
   | '>=' { OpNode (GeOp) }
   | '<=' { OpNode (LeOp) }
   | '&' { OpNode (AndOp) }
   | '|' { OpNode (OrOp) }


-- chunks of declarations
chunks : chunk chunks { 
    let ChunkNode c = $1
        ChunksNode cs = $2
    in ChunksNode (c:cs) }
       | {-empty-} { ChunksNode [] }
chunk : tydecs { Noop }
      | fundecs { Noop }
      | vardec { let VarDeclNode v = $1 in ChunkNode (VarChunk v) }
      | imprt stringLiteral { ChunkNode (ImportChunk $2) }

fundecs : fundec fundecs { Noop }
        | {-empty-} { Noop }

tydecs : tydec tydecs { Noop }
       | {-empty-} { Noop }

vardec : var id ':=' exp { VarDeclNode (VarDecl $2 Nothing $4) }
       | var id ':' typeId ':=' exp { VarDeclNode (VarDecl $2 (Just $4) $6) }

-- type declaration
tydec : type id '=' ty { TypeDeclNode (TypeDecl $2 $4) }

-- function declaration
fundec : function id '(' tyFields ')' '=' exp { FunDeclNode (Function $2 $4 Nothing $7) }
       | function id '(' tyFields ')' ':' typeId '=' exp { FunDeclNode (Function $2 $4 (Just $7) $9) }
       | primitive id '(' tyFields ')' { FunDeclNode (Primitive $2 $4 Nothing) }
       | primitive id '(' tyFields ')' ':' typeId { FunDeclNode (Primitive $2 $4 (Just $7)) }

-- types
ty : typeId  { TypeNode (IdTy $1) }
   | '{' tyFields '}' { TypeNode (RecordTy $2) } 
   | array of typeId { TypeNode (ArrayTy $3) }
tyFields : id ':' typeId moreTyFields { let TyFieldsNode (TyFields tyList) = $4 in TyFieldsNode (TyFields (($1, $3) : tyList)) } 
         | {-empty-} { TyFieldsNode (TyFields []) }
moreTyFields : ',' id ':' typeId moreTyFields { let TyFieldsNode (TyFields tyList) = $5 in TyFieldsNode (TyFields (($2,$4) : tyList)) } 
             | {-empty-} { TyFieldsNode (TyFields []) }
typeId : id { TyIdNode (TyId $1) }

{


parseError _ = do
    ((AlexPn _ line column), _, _, _) <- alexGetInput
    alexError ("parse error at line " ++ (show line) ++ ", column " ++ (show column))

lexer :: (Token -> Alex a) -> Alex a
lexer f = do
    m <- alexMonadScan
    case m of
        Nothing -> lexer f
        (Just x) -> f x
}