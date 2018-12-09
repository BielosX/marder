module EncoderDecoder where

import qualified Data.ByteString.Lazy as B
import qualified Data.Binary as Binary
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.Read

import Lib
import Ber

noEntry = Left "no entry with specified id"
notInteger = Left "error: expected value type INTEGER"

toInt = maybe notInteger Right . (readMaybe :: String -> Maybe Integer)

mapIntegerValue :: String -> IntegerType -> Either String Ber.Value
mapIntegerValue s JustInteger = do
     v <- maybe notInteger Right $ (readMaybe :: String -> Maybe Int) s
     return $ IntegerValue v
mapIntegerValue s (Range o c) = do
     v <- toInt s
     if v <= c && v >= o then return $ IntegerValue $ (fromIntegral :: Integer -> Int) v
     else Left "error: INTEGER out of range"
mapIntegerValue s (Enum l) = do
     let nameToVal = Map.fromList l
     case Map.lookup s nameToVal of
            Nothing -> do
                v <- toInt s
                return $ IntegerValue $ (fromIntegral :: Integer -> Int) $ v
            (Just value) -> return $ IntegerValue $ (fromIntegral :: Integer -> Int) value

mapValue :: String -> Entry -> Either String Ber.Value
mapValue s (ObjType o) = do
        let t = syntax o
        case t of
            (Integer i) -> mapIntegerValue s i
            _ -> Left "not supported"
mapValue s _ = Left "not supported"

encodeValue :: EntryTree -> String -> AbsId -> Either String B.ByteString
encodeValue tree value absId = do
    entry <- maybe noEntry Right $ getEntry absId tree
    value <- mapValue value entry
    return $ Binary.encode value
