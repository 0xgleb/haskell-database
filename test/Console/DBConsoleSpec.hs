module Console.DBConsoleSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Engine.Types.Table
import Console.DBConsole
import Control.Applicative

spec :: Spec
spec = do
    describe "rmDuplicates" $ do
        it "removes elements of the list that repeat" $ do
            rmDuplicates [1, 2, 3, 4, 3, 5, 1, 1, 6] `shouldBe` [1..6]

main :: IO ()
main = hspec spec
