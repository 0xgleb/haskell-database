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

main :: IO ()
main = hspec spec
