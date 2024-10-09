{
module Lexer(lex) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
<0>    $white+ ;
<0>    if { \s -> If pos 2}
<0>    then { \s -> Then pos 4}
<0>    else { \s -> Else pos 4}
<0>    while { \s -> While pos 5}
<0>    for { \s -> For pos 3}
<0>    to { \s -> To pos 2}
<0>    do { \s -> Do pos 2}
<0>    let { \s -> Let pos 3}
<0>    in { \s -> In pos 2}
<0>    end { \s -> End pos 3}
<0>    of { \s -> Of pos 2}
<0>    break { \s -> Break pos 5}
<0>    nil { \s -> Nil pos 3}
<0>    function { \s -> Function pos 8}
<0>    var { \s -> Var pos 3}
<0>    type { \s -> Type pos 4}
<0>    import { \s -> Import pos 6}
<0>    primitive { \s -> Primitive pos 9}
<0>    "," { \s -> Comma pos 1}
<0>    ":" { \s -> Colon pos 1}
<0>    ";" { \s -> Semicolon pos 1}
<0>    "(" { \s -> LeftParen pos 1}
<0>    ")" { \s -> RightParen pos 1}
<0>    "[" { \s -> LeftSquare pos 1}
<0>    "]" { \s -> RightSquare pos 1}
<0>    "{" { \s -> LeftBrace pos 1}
<0>    "}" { \s -> RightBrace pos 1}
<0>    "." { \s -> Period pos 1}
<0>    "+" { \s -> Plus pos 1}
<0>    "-" { \s -> Minus pos 1}
<0>    "*" { \s -> Star pos 1}
<0>    "/" { \s -> Slash pos 1}
<0>    "=" { \s -> Equal pos 1}
<0>    "<>" { \s -> DoubleAngle pos 2}
<0>    "<" { \s -> LeftAngle pos 1}
<0>    ">" { \s -> RightAngle pos 1}
<0>    "<=" { \s -> LeftAngleEqual pos 2 }
<0>    ">=" { \s -> RightAngleEqual pos 2}
<0>    "&" { \s -> Ampersand pos 1}
<0>    "|" { \s -> Pipe pos 1}
<0>    ":=" { \s -> ColonEqual pos 2}
<0>    $alpha ($digit | $alpha | "_")* { \s -> Id pos (length s) s}
-- TODO: Strings and comments
<0>    $digit+ { \s -> NumberLiteral pos (length s) (read s)}
{

}