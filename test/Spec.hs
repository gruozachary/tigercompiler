import Test.Hspec (hspec, describe, it, shouldBe, shouldNotBe)
import qualified Lexing.Lexer as Lx
import Parsing.Parser
import qualified Parsing.Nodes as Pn
import Semantics.Semant

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

parseString :: String -> Either String Pn.Program
parseString s = Lx.runAlex s parse

main :: IO ()
main = hspec $ do
    describe "lexical analysis" $ do
        it "lex nested comments" $ do
            lexString
                "/* Comment /* Nested comment */ Comment */"
                `shouldBe`
                Right []
        
        it "lex all keywords" $ do
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
        
        it "lex string escapes" $ do
            lexString
                "\"\\a \\b \\f \\n \\r \\t \\v\""
                `shouldBe`
                Right [ Lx.StringLiteral "\\a \\b \\f \\n \\r \\t \\v" ]
        
        it "lex all string escape nums" $ do
            lexString "\"\\ff \\FF \\fF \\123 \\123 \\000 \\377\""
            `shouldBe`
            Right
                [ Lx.StringLiteral "\\ff \\FF \\fF \\123 \\123 \\000 \\377" ]
        
        it "lex number literals vs identifiers" $ do
            lexString "123123 g214343 13323 hello0 hei0"
            `shouldBe`
            Right 
                [ Lx.NumberLiteral 123123, Lx.Id "g214343"
                , Lx.NumberLiteral 13323, Lx.Id "hello0", Lx.Id "hei0"
                ]

    describe "parsing" $ do
        it "parse basic expression" $ do
            parseString "hello = 59"
            `shouldBe`
            Right (
                Pn.ExprProg
                (Pn.OpEx
                        (Pn.LValEx (Pn.IdLV (Pn.Id "hello")))
                        Pn.EqOp
                        (Pn.IntEx 59)))

        describe "parse function declarations" $ do
            it "parse zero parameter function declaration" $ do
                parseString "function five() : int = 5"
                `shouldBe`
                Right (
                    Pn.ChunkProg 
                    [Pn.FunChunk 
                    [Pn.Function 
                    (Pn.Id "five") 
                    (Pn.TyFields [])
                    (Just (Pn.TyId "int")) 
                    (Pn.IntEx 5)]])

            it "parse one parameter function declaration" $ do
                parseString "function addOne(x : int) : int = x + 1"
                `shouldBe`
                Right (
                    Pn.ChunkProg 
                    [Pn.FunChunk 
                    [Pn.Function 
                        (Pn.Id "addOne") 
                        (Pn.TyFields [(Pn.Id "x", Pn.TyId "int")])
                        (Just (Pn.TyId "int")) 
                        (Pn.OpEx (Pn.LValEx (Pn.IdLV (Pn.Id "x"))) Pn.AddOp (Pn.IntEx 1))]])
            
            it "parse two parameter function declaration" $ do
                parseString "function mulTwo (x : int, y : int) : int = x * y"
                `shouldBe`
                Right (
                    Pn.ChunkProg 
                    [Pn.FunChunk 
                    [Pn.Function 
                        (Pn.Id "mulTwo") 
                        (Pn.TyFields [(Pn.Id "x",Pn.TyId "int"),(Pn.Id "y",Pn.TyId "int")])
                        (Just (Pn.TyId "int")) 
                        (Pn.OpEx (Pn.LValEx (Pn.IdLV (Pn.Id "x"))) Pn.MultOp (Pn.LValEx (Pn.IdLV (Pn.Id "y"))))]])

        describe "parse type declarations" $ do
            it "parse array type declaration" $ do
                parseString "type int_array = array of int"
                `shouldBe`
                Right (
                    Pn.ChunkProg [
                        Pn.TypeChunk [
                            Pn.TypeDecl (Pn.Id "int_array") (Pn.ArrayTy (Pn.TyId "int"))]])
        
        describe "parse let expressions" $ do
            it "parse hello world" $ do
                parseString "let var s := \"Hello World\" in print(s) end"
                `shouldBe`
                Right (
                    Pn.ExprProg 
                    (Pn.LetEx 
                        [Pn.VarChunk (Pn.VarDecl (Pn.Id "s") Nothing (Pn.StrEx "Hello World"))] 
                        [Pn.FunCallEx (Pn.Id "print") [Pn.LValEx (Pn.IdLV (Pn.Id "s"))]]))
            
            it "parse use of custom array type" $ do
                parseString "let type int_array = array of int \
                                \var table := int_array[100] of 0 \
                            \in print(table[0]) \
                            \end"
                `shouldBe`
                Right (
                    Pn.ExprProg
                    (Pn.LetEx 
                        [Pn.TypeChunk [Pn.TypeDecl (Pn.Id "int_array") (Pn.ArrayTy (Pn.TyId "int"))],
                            Pn.VarChunk (Pn.VarDecl (Pn.Id "table") Nothing (Pn.ArrayEx (Pn.TyId "int_array") (Pn.IntEx 100) (Pn.IntEx 0)))] 
                        [Pn.FunCallEx (Pn.Id "print") [Pn.LValEx (Pn.ArrLV (Pn.IdLV (Pn.Id "table")) (Pn.IntEx 0))]]))
    
    describe "semantic analysis" $ do
        it "fail variable and function with same name" $ do
            file <- readFile "test/namespace.tiger"
            p <- case parseString file of
                Right p -> pure p
                Left  e -> fail e
            runSemant p `shouldBe` ["function name clashes with existing name"]

        it "fail duplicate declarations" $ do
            file <- readFile "test/redeclare.tiger"
            p <- case parseString file of
                Right p -> pure p
                Left  e -> fail e
            runSemant p `shouldBe` 
                ["type name clashes with existing type",
                    "variable name clashes with existing name",
                    "function name clashes with existing name"]

        it "success N-queens solution" $ do
            file <- readFile "test/nqueens.tiger"
            p <- case parseString file of
                Right p -> pure p
                Left  e -> fail e
            runSemant p `shouldBe` []

        it "success array of records" $ do
            file <- readFile "test/arraysrecords.tiger"
            p <- case parseString file of
                Right p -> pure p
                Left e -> fail e
            runSemant p `shouldBe` []

        it "success array of record loop initialization" $ do
            file <- readFile "test/arrayinitialization.tiger"
            p <- case parseString file of
                Right p -> pure p
                Left e -> fail e
            runSemant p `shouldBe` []
        
        it "success chain assignment" $ do
            file <- readFile "test/voidchaining.tiger"
            p <- case parseString file of
                Right p -> pure p
                Left e -> fail e
            runSemant p `shouldBe` []

        it "success recursively defined type" $ do
            file <- readFile "test/recursivetype.tiger"
            p <- case parseString file of
                Right p -> pure p
                Left e -> fail e
            runSemant p `shouldBe` []

        it "success mutual recursively defined type" $ do
            file <- readFile "test/mutualrectype.tiger"
            p <- case parseString file of
                Right p -> pure p
                Left e -> fail e
            runSemant p `shouldBe` []
        
        it "fail cyclic type declaration" $ do
            file <- readFile "test/cyclictypes.tiger"
            p <- case parseString file of
                Right p -> pure p
                Left e -> fail e
            runSemant p `shouldNotBe` []

        it "success nested function" $ do
            file <- readFile "test/nestedfunction.tiger"
            p <- case parseString file of
                Right p -> pure p
                Left e -> fail e
            runSemant p `shouldBe` []

        it "success assign record to nil" $ do
            file <- readFile "test/nilrecord.tiger"
            p <- case parseString file of
                Right p -> pure p
                Left e -> fail e
            runSemant p `shouldBe` []

        it "success record assignment" $ do
            file <- readFile "test/recordassignment.tiger"
            p <- case parseString file of
                Right p -> pure p
                Left e -> fail e
            runSemant p `shouldBe` []

        it "fail record assignment" $ do
            file <- readFile "test/badrecordassignment.tiger"
            p <- case parseString file of
                Right p -> pure p
                Left e -> fail e
            runSemant p `shouldBe` 
                ["record fields invalid",
                    "record fields invalid",
                    "record fields invalid",
                    "cannot get member from a non-record",
                    "function arguments are not of expected type"]

        it "fail infer type of nil" $ do
            file <- readFile "test/assigntonil.tiger"
            p <- case parseString file of
                Right p -> pure p
                Left e -> fail e
            runSemant p `shouldBe` ["cannot infer type of nil"]