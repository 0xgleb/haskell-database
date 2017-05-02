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
            select ["name1"] (Table [("name1", IntType), ("name2", StringType)] [Row [PolyInt 11, PolyString "str1"], Row [PolyInt 22, PolyString "str2"]]) `shouldBe` 
                (Table [("name1", IntType)] [Row [PolyInt 11], Row [PolyInt 22]])
            select ["name2"] (Table [("name1", IntType), ("name2", StringType)] [Row [PolyInt 11, PolyString "str1"], Row [PolyInt 22, PolyString "str2"]]) `shouldBe` 
                (Table [("name2", StringType)] [Row [PolyString "str1"], Row [PolyString "str2"]])
            select ["name1", "name2"] (Table [("name1", IntType), ("name2", StringType)] [Row [PolyInt 11, PolyString "str1"], Row [PolyInt 22, PolyString "str2"]]) `shouldBe` 
                (Table [("name1", IntType), ("name2", StringType)] [Row [PolyInt 11, PolyString "str1"], Row [PolyInt 22, PolyString "str2"]])
            -- select ["invalid"] (Table [("name1", IntType), ("name2", StringType)] [Row [PolyInt 11, PolyString "str1"], Row [PolyInt 22, PolyString "str2"]]) `shouldBe` Nothing

main :: IO ()
main = hspec spec
