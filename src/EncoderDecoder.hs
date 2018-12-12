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
noEntryRef a = Left $ a ++ "not specified"

getTypeDef :: Entry -> Either String (Type, TypeDefOptionals)
getTypeDef (TypeDef _ t o) = Right $ (t, o)
getTypeDef _ = Left "specified entry is not a type definition"

toInt = maybe notInteger Right . (readMaybe :: String -> Maybe Integer)

constrToIntegerType :: TypeConstraint -> Either String IntegerType
constrToIntegerType None = Right $ JustInteger
constrToIntegerType (StringRange _ _)  = Left "wrong constraint type"
constrToIntegerType (StringExact _) = Left "wrong constraint type"
constrToIntegerType (IntegerRange o c) = Right $ Range o c

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

mapEntryRef :: String -> EntryRef -> TypeConstraint -> EntryTree -> Either String Ber.Frame
mapEntryRef s ref constr t = do
    entry <- maybe (noEntryRef ref) Right $ Map.lookup ref $ nameLookup t
    (eType, optionals) <- getTypeDef entry
    case eType of
        (Integer i) -> do
                    ty <- constrToIntegerType constr
                    fmap integerToFrame $ mapIntegerValue s ty
        _ -> Left "not supported"

integerToFrame :: Ber.Value -> Ber.Frame
integerToFrame v = Frame v Ber.Universal Primitive Nothing

mapFrame :: String -> Entry -> EntryTree -> Either String Ber.Frame
mapFrame s (ObjType o) tree = do
        let t = syntax o
        case t of
            (Integer i) -> fmap integerToFrame $ mapIntegerValue s i
            (EntryRefWithConstraint ref c) -> mapEntryRef s ref c tree
            _ -> Left "not supported"
mapValue s _ _ = Left "not supported"

encodeValue :: EntryTree -> String -> AbsId -> Either String B.ByteString
encodeValue tree value absId = do
    entry <- maybe noEntry Right $ getEntry absId tree
    value <- mapFrame value entry tree
    return $ Binary.encode value
