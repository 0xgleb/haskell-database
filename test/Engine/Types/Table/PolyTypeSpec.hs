module Engine.Types.Table.PolyTypeSpec (main, spec, PolyType(..)) where

import Test.Hspec
import Test.QuickCheck

import Engine.Types.Table.PolyType

instance Arbitrary PolyType where
    arbitrary = do
        bool   <- arbitrary :: Gen Bool
        int    <- arbitrary :: Gen Int
        float  <- arbitrary :: Gen Float
        string <- arbitrary :: Gen String
        elements [PolyBool bool, PolyInt int, PolyFloat float, PolyString string, Invalid]

spec :: Spec
spec = do
    describe "<|>" $ do
        it "returns second argument if the first argument is invalid" $ 
            property $ \x -> Invalid <|> x == x
        it "returns first argument if it's not Invalid" $ 
            property $ \x y -> x /= Invalid ==> x <|> y == x

main :: IO ()
main = hspec spec
