module Engine.Types.Table.TableSpec (main, spec, Table(..)) where

import Test.Hspec
import Test.QuickCheck

import Data.Binary

import Engine.Types.Table.ATypeSpec (AType(..))
import Engine.Types.Table.PolyTypeSpec (PolyType(..))
import Engine.Types.Table.Table

instance Arbitrary Table where
    arbitrary = do
        fields <- arbitrary :: Gen [(String, AType)]
        values <- getValues $ map snd fields
        return $ Table fields [] values
            where
                getRow :: [AType] -> Gen Row
                getRow [] = return $ Row []
                getRow (BoolType:xs)   = Row <$> (((:) <$> (PolyBool   <$> arbitrary :: Gen PolyType)) <*> (unRow <$> getRow xs))
                getRow (IntType:xs)    = Row <$> (((:) <$> (PolyInt    <$> arbitrary :: Gen PolyType)) <*> (unRow <$> getRow xs))
                getRow (FloatType:xs)  = Row <$> (((:) <$> (PolyFloat  <$> arbitrary :: Gen PolyType)) <*> (unRow <$> getRow xs))
                getRow (StringType:xs) = Row <$> (((:) <$> (PolyString <$> arbitrary :: Gen PolyType)) <*> (unRow <$> getRow xs))
                getValues fields = do
                    continue <- arbitrary :: Gen Bool
                    if continue then ((:) <$> getRow fields) <*> getValues fields
                                else return []

spec :: Spec
spec = do
    describe "types" $ do
        it "takes Table and returns list of pairs (name, type)" $ do
            property $ \(Table types pKeys values) -> tableTypes (Table types pKeys values) == types
    describe "values" $ do
        it "takes Table and returns list of rows" $ do
            property $ \(Table types pKeys values) -> tableValues (Table types pKeys values) == values
    describe "instance Binary Table" $ do
        it "holds on property decode . encode == id" $
            property $ \x -> length (tableTypes x) /= 0 ==> (decode (encode x) :: Table) == x
    describe "tableProduct" $ do
        let tables = [ ("table1", Table [("name1", IntType), ("name2", StringType)] ["name1"] [Row [PolyInt 1, PolyString "str1"], Row [PolyInt 2, PolyString "str2"]])
                     , ("table2", Table [("name1", IntType), ("name2", StringType)] ["name2"] [Row [PolyInt 1, PolyString "str1"], Row [PolyInt 2, PolyString "str2"]])
                     ]
        it "takes a list of tables and merges into one huge table" $ do
            tableProduct tables `shouldBe` Table [("table1.name1", IntType), ("table1.name2", StringType), ("table2.name1", IntType), ("table2.name2", StringType)]
                                                 ["table1.name1", "table2.name2"]
                                                 [ Row [PolyInt 1, PolyString "str1", PolyInt 1, PolyString "str1"]
                                                 , Row [PolyInt 1, PolyString "str1", PolyInt 2, PolyString "str2"]
                                                 , Row [PolyInt 2, PolyString "str2", PolyInt 1, PolyString "str1"]
                                                 , Row [PolyInt 2, PolyString "str2", PolyInt 2, PolyString "str2"]
                                                 ]

main :: IO ()
main = hspec spec
