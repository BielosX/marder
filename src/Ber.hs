module Ber where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Binary as Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Bits
import Data.Word
import Text.Read
import Data.Int
import Control.Monad
import System.Endian

data Value = IntegerValue Int |
             StringValue String |
             ObjectIdentifier [Int] deriving (Eq, Show)

data TagClass = Universal |
                Application |
                ContextSpecific |
                Private deriving (Eq, Show)

data TagType = Primitive | Constructed deriving (Eq, Show)

data Frame = Frame {
    value :: Value,
    tagClass :: TagClass,
    tagType :: Ber.TagType,
    tagNumber :: Maybe Int
    } | Sequence [Frame] deriving (Eq, Show)

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

lastBit :: Word8 -> Word8
lastBit = (flip shiftR) 7 . (.&.) 0x80

getNFirst :: Int -> Word8 -> Word8
getNFirst n w = (s 0xFF n) .&. w
    where s = shiftR :: Word8 -> Int -> Word8

getNLast :: Int -> Word8 -> Word8
getNLast n w = (flip shiftR) n ((shiftL 0xFF n) .&. w)

blend :: (Int, Word8) -> Word8 -> ((Int, Word8), Word8)
blend (v, w) a = ((carryBits, getNLast carryBits a), ((flip shiftL) v $ getNFirst toTake a) .|. w)
    where toTake = 7 - v
          carryBits = 8 - toTake

_encodeOnSeven :: [Word8] -> (Int, Word8) -> [Word8] -> [Word8]
_encodeOnSeven [] p a | snd p == 0 = a
                      | fst p > 0 = (snd p):a
                      | otherwise = a
_encodeOnSeven (x:xs) p a | fst p == 7 = _encodeOnSeven (x:xs) (0,0) ((snd p):a)
                          | fst p > 0 = let (u, w) = blend p x in  _encodeOnSeven xs u (w:a)
                          | otherwise = _encodeOnSeven xs (1, lastBit x) ((x .&. 0x7F):a)

encodeOnSeven w = _encodeOnSeven w (0,0) []

markBytes :: [Word8] -> Bool -> [Word8] -> [Word8]
markBytes a _ [] = a
markBytes a True (x:xs) = markBytes (x:a) False xs
markBytes a False (x:xs) = markBytes ((x .|. 0x80):a) False xs

encodeIntegerOnSeven = markBytes [] True . reverse . encodeOnSeven . reverse . skipLeadingZeroes

encodeFirstOIDByte :: [Int] -> Word8
encodeFirstOIDByte [] = 0
encodeFirstOIDByte (x:[]) = (fromIntegral :: Int -> Word8) $ x * 40
encodeFirstOIDByte (x:y:xs) = (fromIntegral :: Int -> Word8) $ x * 40 + y

encodeLongLen :: Int -> PutM ()
encodeLongLen l = do
    let value = skipLeadingZeroes l
    let bytes = (fromIntegral :: Int -> Word8) $ length value
    let b = 0x80 :: Word8
    Binary.put $ b + bytes
    mapM_ Binary.put value

tagClassMask :: TagClass -> Word8
tagClassMask Universal = 0
tagClassMask Application = 0x40
tagClassMask ContextSpecific = 0x80
tagClassMask Private = 0xC0

tagTypeMask :: TagType -> Word8
tagTypeMask Primitive = 0
tagTypeMask Constructed = 0x20

primitiveTagNumber :: Value -> Word8
primitiveTagNumber (IntegerValue _) = 2
primitiveTagNumber (StringValue _) = 4
primitiveTagNumber (ObjectIdentifier _) = 6

getTagNumber :: Frame -> Word8
getTagNumber f = case tagNumber f of
                        Nothing -> primitiveTagNumber $ value f
                        (Just n) -> (fromIntegral :: Int -> Word8) n

toClass :: Word8 -> TagClass
toClass 0 = Universal
toClass 1 = Application
toClass 2 = ContextSpecific
toClass 3 = Private

instance Binary.Binary Frame where
    put f@(Frame value tagClass tagType tagNumber) = do
        if tagType == Primitive then do
            let v = (tagTypeMask Primitive) .|. (tagClassMask tagClass)
            Binary.put $ v .|. (getTagNumber f)
            Binary.put $ value
        else do
            let v = Binary.encode $ value
            let tag = (tagTypeMask Constructed) .|. (tagClassMask tagClass)
            Binary.put $ tag .|. (getTagNumber f)
            Binary.put $ (fromIntegral :: Int64 -> Word8) $ B.length v
            putLazyByteString v

    put (Sequence s) = do
        let v = fmap Binary.encode s
        Binary.put $ (tagTypeMask Constructed) .|. 0x10
        let len = foldr (+) 0 $ fmap B.length v
        if len < 128 then Binary.put $ (fromIntegral :: Int64 -> Int8) len
        else encodeLongLen $ (fromIntegral :: Int64 -> Int) len
        mapM_ putLazyByteString v

    get = do
        t <- lookAhead $ (Binary.get :: Binary.Get Word8)
        let tType = (flip shiftR) 5 $ t .&. 0x20
        let tClass = (flip shiftR) 6 $ t .&. 0xC0
        if tType > 0 then fail "not implemented yet"
        else do
            value <- Binary.get :: Binary.Get Value
            return $ Frame value (toClass tClass) Primitive Nothing

_getOfLen :: [Word8] -> Int -> Binary.Get [Word8]
_getOfLen a 0 = return $ reverse a
_getOfLen a n = do
        t <- Binary.get :: Binary.Get Word8
        _getOfLen (t:a) (n-1)

getOfLen = _getOfLen []

word8ToChar a = toEnum v :: Char
    where v = (fromIntegral :: Word8 -> Int) a

_decodeInt :: Bool -> Int -> [Word8] -> Int
_decodeInt _ _ [] = 0
_decodeInt a n (x:xs) | (x .&. 0x80) > 0 && a  = (-p) * (256 - v) + (_decodeInt True (n-1) xs)
                      | otherwise = p * v + (_decodeInt False (n-1) xs)
    where   p = 256^n
            v = (fromIntegral :: Word8 -> Int) x

decodeInt = _decodeInt True

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

    put (Ber.ObjectIdentifier i) = do
        let first = encodeFirstOIDByte $ take 2 i
        let rest = join $ fmap encodeIntegerOnSeven $ drop 2 i
        let whole = first:rest
        let len = length whole
        if len < 128 then Binary.put $ (fromIntegral :: Int -> Int8) len
        else encodeLongLen len
        mapM_ Binary.put whole

    get = do
        t <- Binary.get :: Binary.Get Word8
        let tagN = t .&. 0x1F
        l <- Binary.get :: Binary.Get Word8
        let len = (fromIntegral :: Word8 -> Int) l
        case tagN of
            0x02 -> do
                v <- getOfLen len
                return $ IntegerValue $ decodeInt (len-1) v
            0x04 -> do
                v <- getOfLen len
                return $ StringValue $ fmap word8ToChar v
            0x06 -> fail "not implemented yet"

