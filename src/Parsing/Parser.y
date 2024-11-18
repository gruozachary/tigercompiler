{
module Parser where

import Lexing.Lexer
}

%name parse
%tokentype { Token }
%error { parseError }
%monad { Alex } { >>= } { return }
%lexer { lexer } { EOF }

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

