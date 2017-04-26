module Engine.Functions.DBSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Engine.Functions.DB

spec :: Spec
spec = do
    describe "toDBPath" $
        it "takes the name of a database and returns relative path to it" $
            property $ \x -> toDBPath x == ("./.databases/" ++ x)

main :: IO ()
main = hspec spec
