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
        return $ Table (fields, values)
            where
                getRow :: [AType] -> Gen Row
                getRow [] = return $ Row []
                getRow (BoolType:xs)   = Row <$> (((:) <$> (PolyBool   <$> arbitrary :: Gen PolyType)) <*> (unwrap <$> getRow xs))
                getRow (IntType:xs)    = Row <$> (((:) <$> (PolyInt    <$> arbitrary :: Gen PolyType)) <*> (unwrap <$> getRow xs))
                getRow (FloatType:xs)  = Row <$> (((:) <$> (PolyFloat  <$> arbitrary :: Gen PolyType)) <*> (unwrap <$> getRow xs))
                getRow (StringType:xs) = Row <$> (((:) <$> (PolyString <$> arbitrary :: Gen PolyType)) <*> (unwrap <$> getRow xs))
                getValues fields = do
                    continue <- arbitrary :: Gen Bool
                    if continue then ((:) <$> getRow fields) <*> getValues fields
                                else return []

spec :: Spec
spec = do
    describe "unwrap" $ do
        it "takes Row and returns list that is wrapped in it" $ do
            property $ \l -> unwrap (Row l) == l
    describe "types" $ do
        it "takes Table and returns list of pairs (name, type)" $ do
            property $ \(Table (tableTypes, tableValues)) -> types (Table (tableTypes, tableValues)) == tableTypes
    describe "values" $ do
        it "takes Table and returns list of rows" $ do
            property $ \(Table (tableTypes, tableValues)) -> values (Table (tableTypes, tableValues)) == tableValues
    describe "instance Binary Table" $ do
        it "holds on property decode . encode == id" $
            property $ \x -> length (types x) /= 0 ==> (decode (encode x) :: Table) == x

main :: IO ()
main = hspec spec
