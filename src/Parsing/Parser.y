{
module Parser where

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
    array { Token Array $$ } 
    if { Token If $$ }
    then { Token Then $$ } 
    else { Token Else $$ } 
    while { Token While $$ } 
    for { Token For $$ } 
    to { Token To $$ } 
    do { Token Do $$ } 
    let { Token Let $$ } 
    in { Token In $$ } 
    end { Token End $$ } 
    of { Token Of $$ } 
    break { Token Break $$ } 
    nil { Token Nil $$ } 
    function { Token Function $$ } 
    var { Token Var $$ } 
    type { Token Type $$ } 
    import { Token Import $$ } 
    primitive { Token Primitive $$ } 
    -- Symbols
    ',' { Token Comma  $$ } 
    ':' { Token Colon $$ } 
    ';' { Token Semicolon $$ } 
    '(' { Token LeftParen $$ } 
    ')' { Token RightParen $$ } 
    '[' { Token LeftSquare $$ } 
    ']' { Token RightSquare $$ } 
    '{' { Token LeftBrace $$ } 
    '}' { Token RightBrace $$ } 
    '.' { Token Period $$ } 
    '+' { Token Plus $$ } 
    '-' { Token Minus $$ } 
    '*' { Token Star $$ } 
    '/' { Token Slash $$ } 
    '=' { Token Equal $$ } 
    '<>' { Token DoubleAngle $$ } 
    '<' { Token LeftAngle $$ } 
    '>' { Token RightAngle $$ } 
    '<=' { Token LeftAngleEqual $$ } 
    '>=' { Token RightAngleEqual $$ } 
    '&' { Token Ampersand $$ } 
    '|' { Token Pipe $$ } 
    ':=' { Token ColonEqual $$ }
    eof { Token EOF $$ }
    -- Non-keyword
    id { Token (Id $$) $$ }
    stringLiteral { Token (StringLiteral $$) $$ }
    numberLiteral { Token (numberLiteral $$) $$ }
    
{
parseError :: Token -> Alex a
parserError _ = do
    ((AlexPosn _ line column), _, _, _) <- alexGetInput
    alexError ("parse error at line " ++ (show line) ++ ", column " ++ (show column))

lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)
}

%%
program : exp
        | chunks

exps : exp moreExps
     | {-empty-}
moreExps : ';' exp moreExps
         | {-empty-}
exp : nil
    | integer
    | StringLiteral
-- array and record creations
    | typeId '[' exp ']' of exp
    | typeId '{' '}'
    | typeId '{' id '=' exp recordSubs '}'
-- variables, field, elements of an array
    | lvalue
-- function call
    | id '(' ')'
    | id '(' args ')'
-- operations
    | '-' exp
    | exp op exp
    | '(' exps ')'
-- assignment
    | lvalue ':=' exp
-- control structures
    | if exp then exp else exp
    | if exp then exp
    | while exp do exp
    | for id ':=' exp to exp do exp
    | break
    | let chunks in expds end

       

args : exp moreArgs

moreArgs : ',' exp moreArgs
         | {-empty-}

recordSubs : ',' id '=' exp recordSubs
           | {-empty-}

lvalue : id
       | lvalue '.' id
       | lvalue '[' exp ']'

op : '+' 
   | '-' 
   | '*' 
   | '/' 
   | '=' 
   | '<>' 
   | '>' 
   | '<' 
   | '>=' 
   | '<=' 
   | '&' 
   | '|'


-- chunks of declarations
chunks : chunk chunks
       | {-empty-}
chunk : tydecs
      | fundecs 
      | vardec 
      | import stringLiteral

fundecs : fundec fundecs
        | {-empty-}

tydecs : tydec tydecs
       | {-empty-}

vardec : var id ':=' exp
       | var id ':' typeId ':=' exp

-- type declaration
tydec : type id '=' ty

-- function declaration
fundec : function id '(' tyFields ')' '=' exp
       | function id '(' tyFields ')' ':' typeId '=' exp
       | primitive id '(' tyFields ')'
       | primitive id '(' tyFields ')' ':' typeId

-- types
ty : typeId 
   | '{' tyFields '}' 
   | array of typeId
tyFields : id ':' typeId moreTyFields 
         | {-empty-}
moreTyFields : ',' id ':' typeId moreTyFields 
             | {-empty-}
typeId : id
