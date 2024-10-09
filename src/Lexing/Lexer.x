{
module Lexing.Lexer(AlexPosn(..), Token, lexString) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
<0>    $white+ ;
<0>    array { \pos s -> Array pos}
<0>    if { \pos s -> If pos}
<0>    then { \pos s -> Then pos}
<0>    else { \pos s -> Else pos}
<0>    while { \pos s -> While pos}
<0>    for { \pos s -> For pos}
<0>    to { \pos s -> To pos}
<0>    do { \pos s -> Do pos}
<0>    let { \pos s -> Let pos}
<0>    in { \pos s -> In pos}
<0>    end { \pos s -> End pos}
<0>    of { \pos s -> Of pos}
<0>    break { \pos s -> Break pos}
<0>    nil { \pos s -> Nil pos}
<0>    function { \pos s -> Function pos}
<0>    var { \pos s -> Var pos}
<0>    type { \pos s -> Type pos}
<0>    import { \pos s -> Import pos}
<0>    primitive { \pos s -> Primitive pos}
<0>    "," { \pos s -> Comma pos}
<0>    ":" { \pos s -> Colon pos}
<0>    ";" { \pos s -> Semicolon pos}
<0>    "(" { \pos s -> LeftParen pos}
<0>    ")" { \pos s -> RightParen pos}
<0>    "[" { \pos s -> LeftSquare pos}
<0>    "]" { \pos s -> RightSquare pos}
<0>    "{" { \pos s -> LeftBrace pos}
<0>    "}" { \pos s -> RightBrace pos}
<0>    "." { \pos s -> Period pos}
<0>    "+" { \pos s -> Plus pos}
<0>    "-" { \pos s -> Minus pos}
<0>    "*" { \pos s -> Star pos}
<0>    "/" { \pos s -> Slash pos}
<0>    "=" { \pos s -> Equal pos}
<0>    "<>" { \pos s -> DoubleAngle pos}
<0>    "<" { \pos s -> LeftAngle pos}
<0>    ">" { \pos s -> RightAngle pos}
<0>    "<=" { \pos s -> LeftAngleEqual pos}
<0>    ">=" { \pos s -> RightAngleEqual pos}
<0>    "&" { \pos s -> Ampersand pos}
<0>    "|" { \pos s -> Pipe pos}
<0>    ":=" { \pos s -> ColonEqual pos}
<0>    $alpha ($digit | $alpha | "_")* { \pos s -> Id pos s}
-- TODO: Strings and comments
<0>    $digit+ { \pos s -> NumberLiteral pos (read s)}
{
data Token
    -- Keywords
    = Array AlexPosn
    | If AlexPosn
    | Then AlexPosn
    | Else AlexPosn
    | While AlexPosn
    | For AlexPosn
    | To AlexPosn
    | Do AlexPosn
    | Let AlexPosn
    | In AlexPosn
    | End AlexPosn
    | Of AlexPosn
    | Break AlexPosn
    | Nil AlexPosn
    | Function AlexPosn
    | Var AlexPosn
    | Type AlexPosn
    | Import AlexPosn
    | Primitive AlexPosn
    -- Symbols
    | Comma AlexPosn 
    | Colon AlexPosn
    | Semicolon AlexPosn
    | LeftParen AlexPosn
    | RightParen AlexPosn
    | LeftSquare AlexPosn
    | RightSquare AlexPosn
    | LeftBrace AlexPosn
    | RightBrace AlexPosn
    | Period AlexPosn
    | Plus AlexPosn
    | Minus AlexPosn
    | Star AlexPosn
    | Slash AlexPosn
    | Equal AlexPosn
    | DoubleAngle AlexPosn
    | LeftAngle AlexPosn
    | RightAngle AlexPosn
    | LeftAngleEqual AlexPosn
    | RightAngleEqual AlexPosn
    | Ampersand AlexPosn
    | Pipe AlexPosn
    | ColonEqual AlexPosn
    -- Non-keyword
    | Id AlexPosn String
    | StringLiteral AlexPosn String
    | NumberLiteral AlexPosn Int 
    deriving (Show)

lexString :: String -> [Token]
lexString = alexScanTokens
}