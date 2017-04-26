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
    describe "toTuple" $ do
        context "when passed list has exactly 2 elements" $ do
            it "returns Just tuple with this 2 elements" $ do
                property $ \x1 x2 -> toTuple ([x1, x2] :: [Int]) == Just (x1, x2)
        context "when passed list has more or less than 2 elements" $ do
            it "returns Nothing" $ do
                property $ \l -> length (l :: [Int]) /= 2 ==> toTuple l == Nothing
    describe "toTruple" $ do
        context "when passed list has exactly 3 elements" $ do
            it "returns Just tuple with this 3 elements" $ do
                property $ \x1 x2 x3 -> toTruple ([x1, x2, x3] :: [Int]) == Just (x1, x2, x3)
        context "when passed list has more or less than 3 elements" $ do
            it "returns Nothing" $ do
                property $ \l -> length (l :: [Int]) /= 3 ==> toTruple l == Nothing
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

main :: IO ()
main = hspec spec
