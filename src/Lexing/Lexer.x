{
module Lexing.Lexer(AlexPosn(..), Token, lexString) where
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]

@id  = $alpha ($digit | $alpha | "_")*
@str = \".*\"
@num = $digit+

tokens :-
<0>          $white+   ;
<0>          array     { simpleTok Array }
<0>          if        { simpleTok If }
<0>          then      { simpleTok Then }
<0>          else      { simpleTok Else }
<0>          while     { simpleTok While }
<0>          for       { simpleTok For }
<0>          to        { simpleTok To }
<0>          do        { simpleTok Do }
<0>          let       { simpleTok Let }
<0>          in        { simpleTok In }
<0>          end       { simpleTok End }
<0>          of        { simpleTok Of }
<0>          break     { simpleTok Break }
<0>          nil       { simpleTok Nil }
<0>          function  { simpleTok Function }
<0>          var       { simpleTok Var }
<0>          type      { simpleTok Type }
<0>          import    { simpleTok Import }
<0>          primitive { simpleTok Primitive }
<0>          ","       { simpleTok Comma }
<0>          ":"       { simpleTok Colon }
<0>          ";"       { simpleTok Semicolon }
<0>          "("       { simpleTok LeftParen }
<0>          ")"       { simpleTok RightParen }
<0>          "["       { simpleTok LeftSquare }
<0>          "]"       { simpleTok RightSquare }
<0>          "{"       { simpleTok LeftBrace }
<0>          "}"       { simpleTok RightBrace }
<0>          "."       { simpleTok Period }
<0>          "+"       { simpleTok Plus }
<0>          "-"       { simpleTok Minus }
<0>          "*"       { simpleTok Star }
<0>          "/"       { simpleTok Slash }
<0>          "="       { simpleTok Equal }
<0>          "<>"      { simpleTok DoubleAngle }
<0>          "<"       { simpleTok LeftAngle }
<0>          ">"       { simpleTok RightAngle }
<0>          "<="      { simpleTok LeftAngleEqual }
<0>          ">="      { simpleTok RightAngleEqual }
<0>          "&"       { simpleTok Ampersand }
<0>          "|"       { simpleTok Pipe }
<0>          ":="      { simpleTok ColonEqual }
<0>          @id       { idTok }
<0>          @str      { stringTok }
<0>          @num      { numberTok }

<0, comment> "/*"      { startComment }
<comment>    "*/"      { endComment }
<comment>    .         ;
{
data TokenData
    -- Keywords
    = Array
    | If
    | Then
    | Else
    | While
    | For
    | To
    | Do
    | Let
    | In
    | End
    | Of
    | Break
    | Nil
    | Function
    | Var
    | Type
    | Import
    | Primitive
    -- Symbols
    | Comma 
    | Colon
    | Semicolon
    | LeftParen
    | RightParen
    | LeftSquare
    | RightSquare
    | LeftBrace
    | RightBrace
    | Period
    | Plus
    | Minus
    | Star
    | Slash
    | Equal
    | DoubleAngle
    | LeftAngle
    | RightAngle
    | LeftAngleEqual
    | RightAngleEqual
    | Ampersand
    | Pipe
    | ColonEqual
    -- Non-keyword
    | Id String
    | StringLiteral String
    | NumberLiteral Int 
    | EOF
    deriving (Show)

alexEOF = do
    (pos, _, _, _) <- alexGetInput
    return $ Just $ Token EOF pos

data Token = Token TokenData AlexPosn deriving (Show)

data AlexUserState = AlexUserState { commentDepth :: Int }
alexInitUserState = AlexUserState { commentDepth = 0 }

simpleTok :: TokenData -> AlexInput -> Int -> Alex (Maybe Token)
simpleTok t (pos, _, _, _) _ = return $ Just $ Token t pos

posStringTok :: (Int -> String -> TokenData) -> AlexInput -> Int -> Alex (Maybe Token)
posStringTok f (pos, _, _, s) l = return $ Just $ Token (f l s) pos

idTok :: AlexInput -> Int -> Alex (Maybe Token)
idTok = posStringTok (\l s -> Id $ take l s)

stringTok :: AlexInput -> Int -> Alex (Maybe Token)
stringTok = posStringTok (\l s -> StringLiteral $ tail $ take (l - 1) s)

numberTok :: AlexInput -> Int -> Alex (Maybe Token)
numberTok = posStringTok (\l s -> NumberLiteral $ read $ take l s)

startComment :: AlexInput -> Int -> Alex (Maybe Token)
startComment _ _ = do
    alexSetStartCode comment
    s <- alexGetUserState
    alexSetUserState (s { commentDepth = commentDepth s + 1})
    return Nothing

endComment :: AlexInput -> Int -> Alex (Maybe Token)
endComment _ _ = do
    s <- alexGetUserState
    alexSetUserState (s { commentDepth = commentDepth s - 1})
    if commentDepth s < 2
        then alexSetStartCode 0 >> return Nothing
        else return Nothing

move :: Alex [Token]
move = do
    m <- alexMonadScan
    case m of
        Nothing            -> move
        Just t@(Token d _) -> case d of
            EOF -> return []
            _   -> do
                ts <- move
                return (t : ts)

lexString :: String -> Either String [Token]
lexString s = runAlex s move 
}