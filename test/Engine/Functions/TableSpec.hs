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
        it "takes types, list of names and list of rows and returns selected fields from this rows" $ do
            select [("name1", IntType), ("name2", StringType)] ["name1"] [Row [PolyInt 11, PolyString "str1"], Row [PolyInt 22, PolyString "str2"]] `shouldBe` [Row [PolyInt 11], Row [PolyInt 22]]
            select [("name1", IntType), ("name2", StringType)] ["name2"] [Row [PolyInt 11, PolyString "str1"], Row [PolyInt 22, PolyString "str2"]] `shouldBe` [Row [PolyString "str1"], Row [PolyString "str2"]]
            select [("name1", IntType), ("name2", StringType)] ["name1", "name2"] [Row [PolyInt 11, PolyString "str1"], Row [PolyInt 22, PolyString "str2"]] `shouldBe` [Row [PolyInt 11, PolyString "str1"], Row [PolyInt 22, PolyString "str2"]]

main :: IO ()
main = hspec spec
