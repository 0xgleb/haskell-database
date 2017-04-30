module Engine.Functions.TableSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Engine.Types.Table
import Engine.Functions.Table

spec :: Spec
spec = do
    describe "toPath" $ do
        it "takes a database name and a table name and returns the relative path to it" $ do
            property $ \x y -> toPath x y == ("./.databases/" ++ x ++ "/" ++ y ++ ".table")
    describe "select" $ do
        let table = Table [("name1", IntType), ("name2", StringType)] [Row [PolyInt 11, PolyString "str1"], Row [PolyInt 22, PolyString "str2"]]
        context "when valid name(s) were passed" $ do
            it "returns new table wrapped in Just with data from selected columns" $ do
                select ["name1"]          table `shouldBe` Just (Table [("name1", IntType)] [Row [PolyInt 11], Row [PolyInt 22]])
                select ["name2"]          table `shouldBe` Just (Table [("name2", StringType)] [Row [PolyString "str1"], Row [PolyString "str2"]])
                select ["name1", "name2"] table `shouldBe` Just table
        context "when invalid name(s) were passed" $ do
            it "returns Nothing" $ do
                select ["name1", "invalid"] table `shouldBe` Nothing
                select ["invalid"]          table `shouldBe` Nothing

main :: IO ()
main = hspec spec
