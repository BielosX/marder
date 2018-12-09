import BerSpec
import LibSpec
import EncoderDecoderSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
        berSpec
        libSpec
        encoderDecoderSpec
