{
module Lexing.Lexer(AlexPosn(..), Token, lexString) where
}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
<0>    $white+ ;
<0>    array { \(pos, _, _, _) _ -> return (Array pos) }
<0>    if { \(pos, _, _, _) _  -> return (If pos) }
<0>    then { \(pos, _, _, _) _  -> return (Then pos) }
<0>    else { \(pos, _, _, _) _  -> return (Else pos) }
<0>    while { \(pos, _, _, _) _  -> return (While pos) }
<0>    for { \(pos, _, _, _) _  -> return (For pos) }
<0>    to { \(pos, _, _, _) _  -> return (To pos) }
<0>    do { \(pos, _, _, _) _  -> return (Do pos) }
<0>    let { \(pos, _, _, _) _  -> return (Let pos) }
<0>    in { \(pos, _, _, _) _  -> return (In pos) }
<0>    end { \(pos, _, _, _) _  -> return (End pos) }
<0>    of { \(pos, _, _, _) _  -> return (Of pos) }
<0>    break { \(pos, _, _, _) _  -> return (Break pos) }
<0>    nil { \(pos, _, _, _) _  -> return (Nil pos) }
<0>    function { \(pos, _, _, _) _  -> return (Function pos) }
<0>    var { \(pos, _, _, _) _  -> return (Var pos) }
<0>    type { \(pos, _, _, _) _  -> return (Type pos) }
<0>    import { \(pos, _, _, _) _  -> return (Import pos) }
<0>    primitive { \(pos, _, _, _) _  -> return (Primitive pos) }
<0>    "," { \(pos, _, _, _) _  -> return (Comma pos) }
<0>    ":" { \(pos, _, _, _) _  -> return (Colon pos) }
<0>    ";" { \(pos, _, _, _) _  -> return (Semicolon pos) }
<0>    "(" { \(pos, _, _, _) _  -> return (LeftParen pos) }
<0>    ")" { \(pos, _, _, _) _  -> return (RightParen pos) }
<0>    "[" { \(pos, _, _, _) _  -> return (LeftSquare pos) }
<0>    "]" { \(pos, _, _, _) _  -> return (RightSquare pos) }
<0>    "{" { \(pos, _, _, _) _  -> return (LeftBrace pos) }
<0>    "}" { \(pos, _, _, _) _  -> return (RightBrace pos) }
<0>    "." { \(pos, _, _, _) _  -> return (Period pos) }
<0>    "+" { \(pos, _, _, _) _  -> return (Plus pos) }
<0>    "-" { \(pos, _, _, _) _  -> return (Minus pos) }
<0>    "*" { \(pos, _, _, _) _  -> return (Star pos) }
<0>    "/" { \(pos, _, _, _) _  -> return (Slash pos) }
<0>    "=" { \(pos, _, _, _) _  -> return (Equal pos) }
<0>    "<>" { \(pos, _, _, _) _  -> return (DoubleAngle pos) }
<0>    "<" { \(pos, _, _, _) _  -> return (LeftAngle pos) }
<0>    ">" { \(pos, _, _, _) _  -> return (RightAngle pos) }
<0>    "<=" { \(pos, _, _, _) _  -> return (LeftAngleEqual pos) }
<0>    ">=" { \(pos, _, _, _) _  -> return (RightAngleEqual pos) }
<0>    "&" { \(pos, _, _, _) _  -> return (Ampersand pos) }
<0>    "|" { \(pos, _, _, _) _  -> return (Pipe pos) }
<0>    ":=" { \(pos, _, _, _) _  -> return (ColonEqual pos) }
<0>    $alpha ($digit | $alpha | "_")* { \(pos, _, _, s) l -> (return . Id pos $ take l s) }
-- -- TODO: comments
<0>    \".*\" { \(pos, _, _, s) l -> return (StringLiteral pos (tail $ init $ take l s)) }
<0>    $digit+ { \(pos, _, _, s) l -> return (NumberLiteral pos (read $ take l s)) }
{

alexEOF = return EOF

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
    | EOF
    deriving (Show)

move :: Alex [Token]
move = do
    t <- alexMonadScan
    case t of
        EOF -> return []
        _   -> do
            ts <- move
            return (t : ts)

lexString :: String -> Either String [Token]
lexString s = runAlex s move 
}