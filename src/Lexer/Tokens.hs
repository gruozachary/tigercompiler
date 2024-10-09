module Tokens(Token(..)) where

data Token
    -- Keywords
    = Array Int Int
    | If Int Int
    | Then Int Int
    | Else Int Int
    | While Int Int
    | For Int Int
    | To Int Int
    | Do Int Int
    | Let Int Int
    | In Int Int
    | End Int Int
    | Of Int Int
    | Break Int Int
    | Nil Int Int
    | Function Int Int
    | Var Int Int
    | Type Int Int
    | Import Int Int
    | Primitive Int Int
    -- Symbols
    | Comma Int Int 
    | Colon Int Int
    | Semicolon Int Int
    | LeftParen Int Int
    | RightParen Int Int
    | LeftSquare Int Int
    | RightSquare Int Int
    | LeftBrace Int Int
    | RightBrace Int Int
    | Period Int Int
    | Plus Int Int
    | Minus Int Int
    | Star Int Int
    | Slash Int Int
    | Equal Int Int
    | DoubleAngle Int Int
    | LeftAngle Int Int
    | RightAngle Int Int
    | LeftAngleEqual Int Int
    | RightAngleEqual Int Int
    | Ampersand Int Int
    | Pipe Int Int
    | ColonEqual Int Int
    -- Non-literal
    | StringLiteral Int Int String
    | NumberLiteral Int Int Int 