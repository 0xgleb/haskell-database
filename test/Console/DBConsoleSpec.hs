module Console.DBConsoleSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Engine.Types.Table
import Console.DBConsole
import Control.Applicative

spec :: Spec
spec = do
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
                eval [] "False" (Row []) `shouldBe` Just (PolyBool False)
                eval [] "12345" (Row []) `shouldBe` Just (PolyInt 12345)
                eval [] "12.34" (Row []) `shouldBe` Just (PolyFloat 12.34)
                eval [] "\"s\"" (Row []) `shouldBe` Just (PolyString "s")
                eval [] "inval" (Row []) `shouldBe` Nothing
        context "when passed a field name" $ do
            it "takes the value of the field from given Row" $ do
                eval [("name", IntType)] "name" (Row [PolyInt 15]) `shouldBe` Just (PolyInt 15)
                eval [("name1", IntType), ("name2", StringType)] "name2" (Row [PolyInt 15, PolyString "str"]) `shouldBe` Just (PolyString "str")

main :: IO ()
main = hspec spec
