module Common.MaybeSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Common.Maybe

prop_head l = safeHead l == Just (head l)
    where
        types = l :: [Int]

spec :: Spec
spec = do
    describe "safeHead" $ do
        context "when the list isn't empty" $ do
            it "returns the first element of the list" $ do
                forAll (listOf1 arbitrary) prop_head
        context "when the list is empty" $ do
            it "returns Nothing" $ do
                safeHead ([] :: [Int]) `shouldBe` Nothing
    describe "maybeBoolToBool" $ do
        context "the argument is Nothing" $ do
            it "returns False" $ do
                maybeBoolToBool Nothing `shouldBe` False
        context "argument is Just boolean" $ do
            it "returns boolean" $ do
                property $ \x -> maybeBoolToBool (Just x) == x
    describe "toPair" $ do
        context "when passed list has exactly 2 elements" $ do
            it "returns Just tuple with this 2 elements" $ do
                property $ \x1 x2 -> toPair ([x1, x2] :: [Int]) == Just (x1, x2)
        context "when passed list has more or less than 2 elements" $ do
            it "returns Nothing" $ do
                property $ \l -> length (l :: [Int]) /= 2 ==> toPair l == Nothing
    describe "toTrine" $ do
        context "when passed list has exactly 3 elements" $ do
            it "returns Just tuple with this 3 elements" $ do
                property $ \x1 x2 x3 -> toTrine ([x1, x2, x3] :: [Int]) == Just (x1, x2, x3)
        context "when passed list has more or less than 3 elements" $ do
            it "returns Nothing" $ do
                property $ \l -> length (l :: [Int]) /= 3 ==> toTrine l == Nothing

main :: IO ()
main = hspec spec
