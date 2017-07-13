module Console.StringParsingSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Engine.Types.Table
import Console.StringParsing

spec :: Spec
spec = do
    describe "polyRead" $ do
        context "when passed string is valid" $ do
            it "if BoolType was passed it parses string and returns PolyBool" $
                property $ \x -> polyRead BoolType (show x) == PolyBool x
            it "if IntType was passed it parses string and returns PolyInt" $
                property $ \x -> polyRead IntType (show x) == PolyInt x
            it "if FloatType was passed it parses string and returns PolyFloat" $
                property $ \x -> polyRead FloatType (show x) == PolyFloat x
            it "if StringType was passed it parses string and returns PolyString" $
                property $ \x -> polyRead StringType (show x) == PolyString x
        context "when passed string is invalid" $ do
            it "returns Invalid" $ do
                polyRead BoolType "trua" `shouldBe` Invalid
                polyRead IntType "123_123" `shouldBe` Invalid
                polyRead FloatType "12-3" `shouldBe` Invalid
                polyRead StringType "Test" `shouldBe` Invalid
    describe "parse" $ do
        it "takes a list of types and a list strings and parse each string with respective type" $ do
            property $ \b i f s -> parse [BoolType, IntType, FloatType, StringType] [show b, show i, show f, show s] == [PolyBool b, PolyInt i, PolyFloat f, PolyString s]
    describe "toType" $ do
        it "takes a pair of strings, parses second string as a type and returns new tuple" $ do
            toType ("name", "Bool")   `shouldBe` ("name", BoolType)
            toType ("name", "Int")    `shouldBe` ("name", IntType)
            toType ("name", "Float")  `shouldBe` ("name", FloatType)
            toType ("name", "String") `shouldBe` ("name", StringType)
    describe "readSomething" $ do
        it "tries to parse string to with every possible type of PolyType and returns the first successful parse" $ do
            readSomething "True"    `shouldBe` PolyBool True
            readSomething "1234"    `shouldBe` PolyInt 1234
            readSomething "12.34"   `shouldBe` PolyFloat 12.34
            readSomething "\"str\"" `shouldBe` PolyString "str"
            readSomething "invalid" `shouldBe` Invalid
    describe "toBinOp" $ do
        it "takes string, parses it and returns a binary operator" $ do
            toBinOp "==" <*> (Just 1) <*> (Just 1) `shouldBe` Just True
            toBinOp "<"  <*> (Just 1) <*> (Just 2) `shouldBe` Just True
            toBinOp "<=" <*> (Just 1) <*> (Just 2) `shouldBe` Just True
            toBinOp "<=" <*> (Just 1) <*> (Just 1) `shouldBe` Just True
            toBinOp ">"  <*> (Just 2) <*> (Just 1) `shouldBe` Just True
            toBinOp ">=" <*> (Just 2) <*> (Just 1) `shouldBe` Just True
            toBinOp ">=" <*> (Just 1) <*> (Just 1) `shouldBe` Just True
            toBinOp "/=" <*> (Just 1) <*> (Just 0) `shouldBe` Just True
            toBinOp "a?" <*> (Just 1) <*> (Just 0) `shouldBe` Nothing
    describe "eval" $ do
        context "when passed a value" $ do
            it "parses the value using readSomething" $ do
                eval [] "False" (Row []) `shouldBe` PolyBool False
                eval [] "12345" (Row []) `shouldBe` PolyInt 12345
                eval [] "12.34" (Row []) `shouldBe` PolyFloat 12.34
                eval [] "\"s\"" (Row []) `shouldBe` PolyString "s"
                eval [] "inval" (Row []) `shouldBe` Invalid
        context "when passed a field name" $ do
            it "takes the value of the field from given Row" $ do
                eval [("name", IntType)] "name" (Row [PolyInt 15]) `shouldBe` PolyInt 15
                eval [("name1", IntType), ("name2", StringType)] "name2" (Row [PolyInt 15, PolyString "str"]) `shouldBe` PolyString "str"
    describe "parseGetQuery" $ do
        let table = Table [("id", IntType), ("name", StringType)] ["id"] [Row [PolyInt 1, PolyString "name1"], Row [PolyInt 2, PolyString "name2"]]
        context "when an invalid command was passed" $ do
            it "returns Nothing" $ do
                parseGetQuery [("inval", "something")] table `shouldBe` Nothing
        context "when select is used" $ do
            context "when passed valid name(s)" $ do
                it "returns result of select function applied on the table" $ do
                    parseGetQuery [("select", "id")]         table `shouldBe` Just (Table [("id", IntType)] ["id"] [Row [PolyInt 1], Row [PolyInt 2]])
                    parseGetQuery [("select", "(id, name)")] table `shouldBe` Just table
            context "when  passed invalid name(s)" $ do
                it "returns Nothing" $ do
                    parseGetQuery [("select", "invalid?")] table `shouldBe` Nothing
                    parseGetQuery [("select", "(id, a?)")] table `shouldBe` Nothing
        context "when where is used" $ do
            context "when passed a valid where expression" $ do
                it "returns result of select function applied on the table" $ do
                    parseGetQuery [("where", "(id /= 1)")]           table `shouldBe` Just (Table [("id", IntType), ("name", StringType)] ["id"] [Row [PolyInt 2, PolyString "name2"]])
                    parseGetQuery [("where", "(name == \"name1\")")] table `shouldBe` Just (Table [("id", IntType), ("name", StringType)] ["id"] [Row [PolyInt 1, PolyString "name1"]])
            context "when passed an invalid where expression" $ do
                it "returns Nothing" $ do
                    parseGetQuery [("where", ">= 1234")] table `shouldBe` Nothing
                    parseGetQuery [("where", "sd >= 0")] table `shouldBe` Nothing
                    parseGetQuery [("where", "0 >= sd")] table `shouldBe` Nothing
                    parseGetQuery [("where", "0 ?? id")] table `shouldBe` Nothing
        context "when several functions were used" $ do
            it "composes functions starting from the end of the list" $ do
                parseGetQuery [("select", "name"), ("where", "(id /= 1)")]           table `shouldBe` Just (Table [("name", StringType)] [] [Row [PolyString "name2"]])
                parseGetQuery [("where", "(name == \"name1\")"), ("select", "name")] table `shouldBe` Just (Table [("name", StringType)] [] [Row [PolyString "name1"]])
                parseGetQuery [("where", "(name == \"name1\")"), ("select", "name"), ("where", "(id /= 0)"), ("select", "(id, name)")] table `shouldBe` Just (Table [("name", StringType)] [] [Row [PolyString "name1"]])

main :: IO ()
main = hspec spec
