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
                    [ Lx.Array, Lx.If, Lx.Then, Lx.Else, Lx.While, Lx.For
                    , Lx.To, Lx.Do, Lx.Let, Lx.In, Lx.End, Lx.Of, Lx.Break
                    , Lx.Nil, Lx.Function, Lx.Var, Lx.Type, Lx.Import
                    , Lx.Primitive
                    ]
        
        it "can lex string escapes" $ do
            Lx.lexString
                "\"\\a \\b \\f \\n \\r \\t \\v\""
                `shouldBe`
                Right [ Lx.StringLiteral "\\a \\b \\f \\n \\r \\t \\v" ]
        
        it "can lex all string escape nums" $ do
            Lx.lexString "\"\\ff \\FF \\fF \\123 \\123 \\000 \\377\""
            `shouldBe`
            Right
                [ Lx.StringLiteral "\\ff \\FF \\fF \\123 \\123 \\000 \\377" ]
        
        it "can lex number literals vs identifiers" $ do
            Lx.lexString "123123 g214343 13323 hello0 hei0"
            `shouldBe`
            Right 
                [ Lx.NumberLiteral 123123, Lx.Id "g214343"
                , Lx.NumberLiteral 13323, Lx.Id "hello0", Lx.Id "hei0"
                ]
