module Common.StringSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Common.String

spec :: Spec
spec = do
    describe "split" $ do
        context "when the character is bracket or quote or string doesn't have brackets or quotes" $ do
            it "normally splits strings" $ do
                split 'a' "qwerasdfazxcv" `shouldBe` ["qwer", "sdf", "zxcv"]
                split '_' "hello_world_from_tests" `shouldBe` ["hello", "world", "from", "tests"]
                split '(' "test(test" `shouldBe` ["test", "test"]
                split '"' "test\"test" `shouldBe` ["test", "test"]
        context "when string has brackets and/or quotes" $ do
            it "splits strings without looking inside brackets and quotes" $ do
                split ' ' "john said: \"hello guys\"" `shouldBe` ["john", "said:", "\"hello guys\""]
                split ' ' "with brackets (hello world)" `shouldBe` ["with", "brackets", "(hello world)"]
                split ' ' "\"it's a\" (mixed test)" `shouldBe` ["\"it's a\"", "(mixed test)"]
    describe "rm" $ do
        context "when string doesn't have brackets or quotes" $ do
            it "removes the character from the string" $ do
                rm 'a' "qwerasdfazxcv" `shouldBe` "qwersdfzxcv"
                rm ' ' "hello world" `shouldBe` "helloworld"
        context "when string has brackets and/or quotes" $ do
            it "removed the character from the string if it's not inside brackets or quotes" $ do
                rm ' ' "test \"with quotes\"" `shouldBe` "test\"with quotes\""
                rm ' ' "test (with brackets)" `shouldBe` "test(with brackets)"
                rm ' ' "\"mixed test\" (with brackets)" `shouldBe` "\"mixed test\"(with brackets)"

main :: IO ()
main = hspec spec
