import Test.Hspec (hspec, describe, it, shouldBe)
import qualified Lexing.Lexer as Lx

-- This function produces a list of tokens from the input, using alexMonadScan
move :: Lx.Alex [Lx.Token]
move = do
    m <- Lx.alexMonadScan
    case m of
        Nothing            -> move
        Just t -> case t of
            Lx.EOF -> return []
            _   -> do
                ts <- move
                return (t : ts)

-- Runs the alex monad
lexString :: String -> Either String [Lx.Token]
lexString s = Lx.runAlex s move 

main :: IO ()
main = hspec $ do
    describe "lexString" $ do
        it "can lex nested comments" $ do
            lexString
                "/* Comment /* Nested comment */ Comment */"
                `shouldBe`
                Right []
        
        it "can lex all keywords" $ do
            lexString
                "array if then else while for to do let in end of break nil \
                \function var type import primitive"
                `shouldBe`
                Right
                    [ Lx.Array, Lx.If, Lx.Then, Lx.Else, Lx.While, Lx.For
                    , Lx.To, Lx.Do, Lx.Let, Lx.In, Lx.End, Lx.Of, Lx.Break
                    , Lx.Nil, Lx.Function, Lx.Var, Lx.Type, Lx.Import
                    , Lx.Primitive
                    ]
        
        it "can lex string escapes" $ do
            lexString
                "\"\\a \\b \\f \\n \\r \\t \\v\""
                `shouldBe`
                Right [ Lx.StringLiteral "\\a \\b \\f \\n \\r \\t \\v" ]
        
        it "can lex all string escape nums" $ do
            lexString "\"\\ff \\FF \\fF \\123 \\123 \\000 \\377\""
            `shouldBe`
            Right
                [ Lx.StringLiteral "\\ff \\FF \\fF \\123 \\123 \\000 \\377" ]
        
        it "can lex number literals vs identifiers" $ do
            lexString "123123 g214343 13323 hello0 hei0"
            `shouldBe`
            Right 
                [ Lx.NumberLiteral 123123, Lx.Id "g214343"
                , Lx.NumberLiteral 13323, Lx.Id "hello0", Lx.Id "hei0"
                ]
