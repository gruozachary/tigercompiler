{
module Lexing.Lexer(AlexPosn(..), Token, lexString) where
}

%wrapper "monadUserState"

$digit = 0-9
$oct   = 0-7
$hex   = [0-9a-fA-F]
$alpha = [a-zA-Z]

@id   = $alpha ($digit | $alpha | "_")*
@num  = $digit+
@slsh = [abfnrtv\\\"] | 0-30-70-7 | $hex$hex 

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
<0>          @num      { numberTok }

<0, comment> "/*"      { startComment }
<comment>    "*/"      { endComment }
<comment>    .         ;

<0>          \"        { beginString }
<string,sp>  \"        { endString }
<string>     \\        { beginBackslash }
<string>     [^\"]     { stringChar }

<bslash>     @slsh     { backslashChar }

<sp>         " "       { space }
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

-- Alex requires implementation to show what to do when EOF is encountered
alexEOF = do
    (pos, _, _, _) <- alexGetInput
    return $ Just $ Token EOF pos

-- A token type to encapsulate postion information and the kind of token
data Token = Token TokenData AlexPosn deriving (Show)

-- Custom state defined for use with the monadUserState wrapper
data AlexUserState = AlexUserState
    { commentDepth :: Int 
    , stringValue  :: String
    }

-- Required function by Alex to initialise the state
alexInitUserState = AlexUserState 
    { commentDepth = 0
    , stringValue  = ""
    }

-- A simple function for raising a token that takes no constructors into the Alex monadic context
simpleTok :: TokenData -> AlexInput -> Int -> Alex (Maybe Token)
simpleTok t (pos, _, _, _) _ = return $ Just $ Token t pos

-- A helper function for raising more complex tokens into the Alex monadic context
posStringTok :: (Int -> String -> TokenData) -> AlexInput -> Int -> Alex (Maybe Token)
posStringTok f (pos, _, _, s) l = return $ Just $ Token (f l s) pos

-- Handles ID tokens
idTok :: AlexInput -> Int -> Alex (Maybe Token)
idTok = posStringTok (\l s -> Id $ take l s)

-- Handles number literals
numberTok :: AlexInput -> Int -> Alex (Maybe Token)
numberTok = posStringTok (\l s -> NumberLiteral $ read $ take l s)

-- Puts the lexer into comment mode.
-- Keeps track of the nested comment depth and sets the start code
startComment :: AlexInput -> Int -> Alex (Maybe Token)
startComment _ _ = do
    alexSetStartCode comment
    s <- alexGetUserState
    alexSetUserState (s { commentDepth = commentDepth s + 1})
    return Nothing

-- Tries to take the lexer out of comment mode if the depth is correct
endComment :: AlexInput -> Int -> Alex (Maybe Token)
endComment _ _ = do
    s <- alexGetUserState
    alexSetUserState (s { commentDepth = commentDepth s - 1})
    if commentDepth s < 2
        then alexSetStartCode 0 >> return Nothing
        else return Nothing

-- Puts the lexer in string mode for consuming string literals
-- Resets the string state value
beginString :: AlexInput -> Int -> Alex (Maybe Token)
beginString _ _ = do
    alexSetStartCode string
    u <- alexGetUserState
    alexSetUserState $ u { stringValue = ""}
    return Nothing

-- Ends the string and creates a string literal token
endString :: AlexInput -> Int -> Alex (Maybe Token)
endString (pos, _, _, _) _ = do
    s <- alexGetUserState
    alexSetStartCode 0
    return $ Just $ Token (StringLiteral $ stringValue s) pos

-- Adds a new character to the string in our state
stringChar :: AlexInput -> Int -> Alex (Maybe Token) 
stringChar (_, _, _, s) l = do
    u <- alexGetUserState
    alexSetUserState $ u { stringValue = (stringValue u) ++ (take l s)}
    return Nothing

-- Begins the backslash operation inside of the string operation of the lexer
beginBackslash :: AlexInput -> Int -> Alex (Maybe Token)
beginBackslash  (_, _, _, s) l = do
    alexSetStartCode bslash
    u <- alexGetUserState
    alexSetUserState $ u { stringValue = (stringValue u) ++ (take l s)}
    return Nothing

-- Similar to stringChar, but makes sure that if the string is not ended straight after, a space appears
backslashChar :: AlexInput -> Int -> Alex (Maybe Token)
backslashChar i l = alexSetStartCode sp >> stringChar i l

-- Consumes a space character
space :: AlexInput -> Int -> Alex (Maybe Token)
space i l = alexSetStartCode string >> stringChar i l 

-- This function produces a list of tokens from the input, using alexMonadScan
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

-- Runs the alex monad
lexString :: String -> Either String [Token]
lexString s = runAlex s move 
}