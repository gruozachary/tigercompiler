{
module Parsing.Parser where

import Lexing.Lexer
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
program : exp { Noop }
        | chunks { Noop }

exps : exp moreExps { Noop }
     | {-empty-} { Noop }
moreExps : ';' exp moreExps { Noop }
         | {-empty-} { Noop }
exp : nil { Noop }
    | numberLiteral { Noop }
    | stringLiteral { Noop }
-- array and record creations
    | typeId '[' exp ']' of exp { Noop }
    | typeId '{' '}' { Noop }
    | typeId '{' id '=' exp recordSubs '}' { Noop }
-- variables, field, elements of an array
    | lvalue { Noop }
-- function call
    | id '(' ')' { Noop }
    | id '(' args ')' { Noop }
-- operations
    | '-' exp { Noop }
    | exp op exp { Noop }
    | '(' exps ')' { Noop }
-- assignment
    | lvalue ':=' exp { Noop }
-- control structures
    | if exp then exp else exp { Noop }
    | if exp then exp { Noop }
    | while exp do exp { Noop }
    | for id ':=' exp to exp do exp { Noop }
    | break { Noop }
    | let chunks in exps end { Noop }

       

args : exp moreArgs { Noop }

moreArgs : ',' exp moreArgs { Noop }
         | {-empty-} { Noop }

recordSubs : ',' id '=' exp recordSubs { Noop }
           | {-empty-} { Noop }

lvalue : id { Noop }
       | lvalue '.' id { Noop }
       | lvalue '[' exp ']' { Noop }

op : '+' { Noop }
   | '-' { Noop }
   | '*' { Noop }
   | '/' { Noop }
   | '=' { Noop }
   | '<>' { Noop }
   | '>' { Noop }
   | '<' { Noop }
   | '>=' { Noop }
   | '<=' { Noop }
   | '&' { Noop }
   | '|' { Noop }


-- chunks of declarations
chunks : chunk chunks { Noop }
       | {-empty-} { Noop }
chunk : tydecs { Noop }
      | fundecs { Noop }
      | vardec { Noop }
      | imprt stringLiteral { Noop }

fundecs : fundec fundecs { Noop }
        | {-empty-} { Noop }

tydecs : tydec tydecs { Noop }
       | {-empty-} { Noop }

vardec : var id ':=' exp { Noop }
       | var id ':' typeId ':=' exp { Noop }

-- type declaration
tydec : type id '=' ty { Noop }

-- function declaration
fundec : function id '(' tyFields ')' '=' exp { Noop }
       | function id '(' tyFields ')' ':' typeId '=' exp { Noop }
       | primitive id '(' tyFields ')' { Noop }
       | primitive id '(' tyFields ')' ':' typeId { Noop }

-- types
ty : typeId  { Noop }
   | '{' tyFields '}' { Noop } 
   | array of typeId { Noop }
tyFields : id ':' typeId moreTyFields { Noop } 
         | {-empty-} { Noop }
moreTyFields : ',' id ':' typeId moreTyFields { Noop } 
             | {-empty-} { Noop }
typeId : id { Noop }

{
data Node = Noop

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