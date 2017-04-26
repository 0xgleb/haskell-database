module Console.DBConsoleSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Engine.Types.Table
import Console.DBConsole

spec :: Spec
spec = do
    describe "toBinOp" $ do
        it "takes string, parses it and returns a binary operator" $ do
            toBinOp "==" 1 1 `shouldBe` True
            toBinOp "<"  1 2 `shouldBe` True
            toBinOp "<=" 1 2 `shouldBe` True
            toBinOp "<=" 1 1 `shouldBe` True
            toBinOp ">"  2 1 `shouldBe` True
            toBinOp ">=" 2 1 `shouldBe` True
            toBinOp ">=" 1 1 `shouldBe` True
            toBinOp "/=" 1 0 `shouldBe` True
            toBinOp "a?" 1 1 `shouldBe` False
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

main :: IO ()
main = hspec spec
