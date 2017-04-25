module Engine.Types.Table.ATypeSpec (main, spec, AType(..)) where

import Test.Hspec
import Test.QuickCheck

import Data.Binary
import qualified Data.ByteString.Lazy as BL
import Engine.Types.Table.AType

instance Arbitrary AType where
    arbitrary = elements possibleTypes

spec :: Spec
spec = do
    describe "instance Binary AType" $ do
        it "holds on property decode . encode == id" $
            property $ \x -> (decode (encode x) :: AType) == x

main :: IO ()
main = hspec spec
