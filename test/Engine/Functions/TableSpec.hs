module Engine.Functions.TableSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Engine.Functions.Table

spec :: Spec
spec = do
    describe "toPath" $ do
        it "takes a database name and a table name and returns the relative path to it" $ do
            property $ \x y -> toPath x y == ("./.databases/" ++ x ++ "/" ++ y ++ ".table")
    let table = Table [("name1", IntType), ("name2", StringType)] ["name1"] [Row [PolyInt 11, PolyString "str1"], Row [PolyInt 22, PolyString "str2"]]
        table2 = Table [("one_id", IntType), ("another_id", IntType), ("name", StringType)] ["one_id", "another_id"] [Row [PolyInt 1, PolyInt 2, PolyString "?"], Row [PolyInt 2, PolyInt 1, PolyString "?"]]
    describe "select" $ do
        context "when valid name(s) were passed" $ do
            it "returns new table wrapped in Just with data from selected columns" $ do
                select ["name1"]          table `shouldBe` Just (Table [("name1", IntType)] ["name1"] [Row [PolyInt 11], Row [PolyInt 22]])
                select ["name2"]          table `shouldBe` Just (Table [("name2", StringType)] [] [Row [PolyString "str1"], Row [PolyString "str2"]])
                select ["name1", "name2"] table `shouldBe` Just table
        context "when invalid name(s) were passed" $ do
            it "returns Nothing" $ do
                select ["name1", "invalid"] table `shouldBe` Nothing
                select ["invalid"]          table `shouldBe` Nothing
    describe "searchTable" $ do
        it "returns Just Row if contains a row with the given primary keys" $ do
            searchTable (PrimaryKeyValues [PolyInt 11]) table `shouldBe` Just (Row [PolyInt 11, PolyString "str1"])
            searchTable (PrimaryKeyValues [PolyInt 22]) table `shouldBe` Just (Row [PolyInt 22, PolyString "str2"])
        it "returns Nothing if an invalid set of primary keys was given" $ do
            searchTable (PrimaryKeyValues []) table `shouldBe` Nothing
            searchTable (PrimaryKeyValues [PolyString "str1"]) table `shouldBe` Nothing
            searchTable (PrimaryKeyValues [PolyString "str2"]) table `shouldBe` Nothing
            searchTable (PrimaryKeyValues [PolyInt 5]) table `shouldBe` Nothing
            searchTable (PrimaryKeyValues [PolyInt 22, PolyString "str2"]) table `shouldBe` Nothing
        it "returns Nothing if it couldn't find a matching row" $ do
            searchTable (PrimaryKeyValues [PolyString "str3"]) table `shouldBe` Nothing
    describe "getPKValues" $ do
        it "returns each row together with values of primary keys from that row" $ do
            getPKValues table `shouldBe` [(Row [PolyInt 11, PolyString "str1"], PrimaryKeyValues [PolyInt 11]), (Row [PolyInt 22, PolyString "str2"], PrimaryKeyValues [PolyInt 22])]
            getPKValues table2 `shouldBe` [ (Row [PolyInt 1, PolyInt 2, PolyString "?"], PrimaryKeyValues [PolyInt 1, PolyInt 2])
                                          , (Row [PolyInt 2, PolyInt 1, PolyString "?"], PrimaryKeyValues [PolyInt 2, PolyInt 1])
                                          ]

main :: IO ()
main = hspec spec
