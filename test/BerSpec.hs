module BerSpec where

import Test.Hspec

import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Char

import Ber

cToW8 :: Char -> Word8
cToW8 = (fromIntegral :: Int -> Word8) . ord

encoded = B.unpack . encode

decodeFrame = decode :: B.ByteString -> Frame

berSpec = do
            describe "encode" $ do
                it "should encode primitive integer type" $ do
                    let v = IntegerValue 5
                    encoded v `shouldBe` [1, 5]

                it "should encode octet string" $ do
                    let v = StringValue "xvi"
                    encoded v `shouldBe` [3, cToW8 'x', cToW8 'v', cToW8 'i']

                it "should encode simple OID" $ do
                    let v = ObjectIdentifier [1,2,3]
                    encoded v `shouldBe` [2, 42, 3]

                it "should encode OID with big number" $ do
                    let v = ObjectIdentifier [1,2,500]
                    encoded v `shouldBe` [3, 42, 0x83, 0x74]

            describe "decode" $ do
                it "should decode octet string" $ do
                    let v = B.pack [4, 3, cToW8 'x', cToW8 'v', cToW8 'i']
                    let f = Frame (StringValue "xvi") Universal Primitive Nothing
                    decodeFrame v `shouldBe` f

                it "should decode positive integer value" $ do
                    let v = B.pack [2, 2, 0x01, 0xFF]
                    let f = Frame (IntegerValue 511) Universal Primitive Nothing
                    decodeFrame v `shouldBe` f

                it "should decode negative integer value" $ do
                    let v = B.pack [2, 2, 0xFF, 0x7F]
                    let f = Frame (IntegerValue (-129)) Universal Primitive Nothing
                    decodeFrame v `shouldBe` f

                it "should decode one byte negative integer value" $ do
                    let v = B.pack [2, 1, 0x81]
                    let f = Frame (IntegerValue (-127)) Universal Primitive Nothing
                    decodeFrame v `shouldBe` f

