module Ber where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Binary as Binary
import Data.Binary.Put
import Data.Bits
import Data.Word
import Text.Read
import Data.Int

import Lib

data Value = IntegerValue Int |
             StringValue String deriving (Eq, Show)

negative x = (.&.) x 0x80 > 0
positive = not . negative

removeRedundancy :: [Word8] -> [Word8]
removeRedundancy [] = []
removeRedundancy (x:[]) = [x]
removeRedundancy (x:y:xs) | x == 0xFF && negative y  = removeRedundancy (y:xs)
                          | x == 0xFF && positive y  = x:(removeRedundancy (y:xs))
                          | x == 0 && y == 0xFF = removeRedundancy (y:xs)
                          | x == 0 && negative y = 0:removeRedundancy (y:xs)
                          | x == 0 && positive y = removeRedundancy (y:xs)
                          | otherwise = x:y:xs

_encodeInteger :: Int -> [Word8]
_encodeInteger = removeRedundancy . B.unpack . Binary.encode

skipLeadingZeroes = dropWhile (\x -> x == 0) . B.unpack . Binary.encode

encodeLongLen :: Int -> PutM ()
encodeLongLen l = do
    let value = skipLeadingZeroes l
    let bytes = (fromIntegral :: Int -> Word8) $ length value
    let b = 0x80 :: Word8
    Binary.put $ b + bytes
    mapM_ Binary.put value

instance Binary.Binary Value where
    put (IntegerValue i) = do
        let value = _encodeInteger i
        Binary.put $ (fromIntegral :: Int -> Int8) $ length value
        mapM_ Binary.put value

    put (StringValue s) = do
        let value = C.pack s
        if B.length value < 128 then
            Binary.put $ (fromIntegral :: Int64 -> Int8) $ B.length value
        else encodeLongLen $ (fromIntegral :: Int64 -> Int) $ B.length value
        putLazyByteString value

    get = fail "not implemented yet"

