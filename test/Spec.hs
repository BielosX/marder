import BerSpec
import LibSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
        berSpec
        libSpec
