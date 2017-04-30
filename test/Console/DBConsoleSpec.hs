module Console.DBConsoleSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Engine.Types.Table
import Console.DBConsole
import Control.Applicative

spec :: Spec
spec = return ()

main :: IO ()
main = hspec spec
