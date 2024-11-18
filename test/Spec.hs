import Test.Hspec (hspec, describe, it, shouldBe)
import qualified Lexing.Lexer as Lx

main :: IO ()
main = hspec $ do
    describe "lexString" $ do
        it "can lex nested comments" $ do
            Lx.lexString
                "/* Comment /* Nested comment */ Comment */"
                `shouldBe`
                Right []
        
        it "can lex all keywords" $ do
            Lx.lexString
                "array if then else while for to do let in end of break nil \
                \function var type import primitive"
                `shouldBe`
                Right
                    [ Lx.Token Lx.Array (Lx.AlexPn 0 1 1)
                    , Lx.Token Lx.If (Lx.AlexPn 6 1 7)
                    , Lx.Token Lx.Then (Lx.AlexPn 9 1 10)
                    , Lx.Token Lx.Else (Lx.AlexPn 14 1 15)
                    , Lx.Token Lx.While (Lx.AlexPn 19 1 20)
                    , Lx.Token Lx.For (Lx.AlexPn 25 1 26)
                    , Lx.Token Lx.To (Lx.AlexPn 29 1 30)
                    , Lx.Token Lx.Do (Lx.AlexPn 32 1 33)
                    , Lx.Token Lx.Let (Lx.AlexPn 35 1 36)
                    , Lx.Token Lx.In (Lx.AlexPn 39 1 40)
                    , Lx.Token Lx.End (Lx.AlexPn 42 1 43)
                    , Lx.Token Lx.Of (Lx.AlexPn 46 1 47)
                    , Lx.Token Lx.Break (Lx.AlexPn 49 1 50)
                    , Lx.Token Lx.Nil (Lx.AlexPn 55 1 56)
                    , Lx.Token Lx.Function (Lx.AlexPn 59 1 60)
                    , Lx.Token Lx.Var (Lx.AlexPn 68 1 69)
                    , Lx.Token Lx.Type (Lx.AlexPn 72 1 73)
                    , Lx.Token Lx.Import (Lx.AlexPn 77 1 78)
                    , Lx.Token Lx.Primitive (Lx.AlexPn 84 1 85)
                    ]
        
        it "can lex string escapes" $ do
            Lx.lexString
                "\"\\a \\b \\f \\n \\r \\t \\v\""
                `shouldBe`
                Right
                    [ Lx.Token
                        (Lx.StringLiteral "\\a \\b \\f \\n \\r \\t \\v")
                        (Lx.AlexPn 21 1 22)
                    ]
        
        it "can lex all string escape nums" $ do
            Lx.lexString "\"\\ff \\FF \\fF \\123 \\123 \\000 \\377\""
            `shouldBe`
            Right
                [ Lx.Token
                    (Lx.StringLiteral "\\ff \\FF \\fF \\123 \\123 \\000 \\377")
                    (Lx.AlexPn 32 1 33)
                ]
        
        it "can lex number literals vs identifiers" $ do
            Lx.lexString "123123 g214343 13323 hello0 hei0"
            `shouldBe`
            Right 
                [ Lx.Token (Lx.NumberLiteral 123123) (Lx.AlexPn 0 1 1)
                , Lx.Token (Lx.Id "g214343") (Lx.AlexPn 7 1 8)
                , Lx.Token (Lx.NumberLiteral 13323) (Lx.AlexPn 15 1 16)
                , Lx.Token (Lx.Id "hello0") (Lx.AlexPn 21 1 22)
                , Lx.Token (Lx.Id "hei0") (Lx.AlexPn 28 1 29)
                ]
