module EncoderDecoder where

import qualified Data.ByteString.Lazy as B
import qualified Data.Binary as Binary
import qualified Data.Map.Strict as Map
import Data.List.Split
import Data.Maybe
import Text.Read

import Lib
import Ber

noEntry = Left "no entry with specified id"
notInteger = Left "error: expected value type INTEGER"
noEntryRef a = Left $ a ++ "not specified"
wrongConstr = Left "wrong constraint type"
wrongStrSize s = Left $ "error: STRING size should be exactly " ++ (show s)
strSizeOutOfRange = Left "error: STRING size out of range"
wrongOidFormat = Left "wrong OID format"

getTypeDef :: Entry -> Either String (Type, TypeDefOptionals)
getTypeDef (TypeDef _ t o) = Right $ (t, o)
getTypeDef _ = Left "specified entry is not a type definition"

toInt = maybe notInteger Right . (readMaybe :: String -> Maybe Integer)

constrToIntegerType :: IntegerType -> TypeConstraint -> Either String IntegerType
constrToIntegerType t None = Right $ t
constrToIntegerType _ (StringRange _ _)  = wrongConstr
constrToIntegerType _ (StringExact _) = wrongConstr
constrToIntegerType _ (IntegerRange o c) = Right $ Range o c

constrToOctStringType :: OctetString -> TypeConstraint -> Either String OctetString
constrToOctStringType t None = Right $ t
constrToOctStringType _ (IntegerRange _ _) = wrongConstr
constrToOctStringType _ (StringExact v) = Right $ StrictSize v
constrToOctStringType _ (StringRange o c) = Right $ BoundSize o c

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

strLen = (fromIntegral :: Int -> Integer) . length

strLenInRange s o c | len >= o && len <= c = True
                    | otherwise = False
                    where len = strLen s

mapOctStringValue :: String -> OctetString -> Either String Ber.Value
mapOctStringValue s JustString = Right $ StringValue s
mapOctStringValue s (StrictSize size) | strLen s /= size = wrongStrSize size
                                      | otherwise = Right $ StringValue s
mapOctStringValue s (BoundSize o c) | strLenInRange s o c = Right $ StringValue s
                                    | otherwise = strSizeOutOfRange

mapEntryRef :: String -> EntryRef -> TypeConstraint -> EntryTree -> Either String Ber.Frame
mapEntryRef s ref constr t = do
    entry <- maybe (noEntryRef ref) Right $ Map.lookup ref $ nameLookup t
    (eType, optionals) <- getTypeDef entry
    case eType of
        (Integer i) -> do
                    ty <- constrToIntegerType i constr
                    mapPimitiveWithOpt optionals $ mapIntegerValue s ty
        (OctString v) -> do
                    ty <- constrToOctStringType v constr
                    mapPimitiveWithOpt optionals $ mapOctStringValue s ty
        _ -> Left "not supported"

mapPimitiveWithOpt :: TypeDefOptionals -> (Either String Ber.Value -> Either String Ber.Frame)
mapPimitiveWithOpt optionals = fmap (applyTypeDefOpt optionals . primitiveToFrame)

primitiveToFrame :: Ber.Value -> Ber.Frame
primitiveToFrame v = Frame v Ber.Universal Primitive Nothing

mapTagType :: Maybe Lib.TagType -> Ber.TagType
mapTagType Nothing = Primitive
mapTagType (Just Implicit) = Primitive
mapTagType (Just Explicit) = Constructed

mapVisibility Lib.Universal = Ber.Universal
mapVisibility Lib.Application = Ber.Application
mapVisibility Lib.ContextSpecific = Ber.ContextSpecific
mapVisibility Lib.Private = Ber.Private

stringToOid :: String -> Maybe [Int]
stringToOid = mapM (readMaybe :: String -> Maybe Int) . splitOn "."

applyTypeDefOpt :: TypeDefOptionals -> Ber.Frame -> Ber.Frame
applyTypeDefOpt opt f = f { Ber.tagClass = vis, Ber.tagType = tType, Ber.tagNumber = tNum }
    where vis = fromMaybe (tagClass f) (fmap mapVisibility $ visibility opt)
          tType = mapTagType $ Lib.tagType opt
          tNum = fmap (fromIntegral :: Integer -> Int) (Lib.tagNumber opt)

oidToFrame = primitiveToFrame . Ber.ObjectIdentifier

mapFrame :: String -> Entry -> EntryTree -> Either String Ber.Frame
mapFrame s (ObjType o) tree = do
        let t = syntax o
        case t of
            (Integer i) -> fmap primitiveToFrame $ mapIntegerValue s i
            (EntryRefWithConstraint ref c) -> mapEntryRef s ref c tree
            (OctString v) -> fmap primitiveToFrame $ mapOctStringValue s v
            Lib.ObjectIdentifier -> maybe wrongOidFormat (Right . oidToFrame) $ stringToOid s
            _ -> Left "not supported"
mapValue s _ _ = Left "not supported"

encodeValue :: EntryTree -> String -> AbsId -> Either String B.ByteString
encodeValue tree value absId = do
    entry <- maybe noEntry Right $ getEntry absId tree
    value <- mapFrame value entry tree
    return $ Binary.encode value

validateType :: Type -> Ber.Value -> Either String ()
validateType (Integer _) (IntegerValue _)  = return ()
validateType (Integer _) (StringValue _) = Left "error: expected INTEGER provided OCTET STRING"
validateType (Integer _) (Ber.ObjectIdentifier _) = Left "error: expected INTEGER provided OID"
validateType (OctString _) (IntegerValue _) = Left "error: expected OCTET STRING provided INTEGER"
validateType (OctString _) (StringValue _) = return ()
validateType (OctString _) (Ber.ObjectIdentifier _) = Left "error: expected OCTET STRING provided OID"
validateType Lib.ObjectIdentifier (Ber.ObjectIdentifier _) = return ()
validateType Lib.ObjectIdentifier (IntegerValue _) = Left "error: expected OID provided INTEGER"
validateType Lib.ObjectIdentifier (StringValue _) = Left "error: expected OID provided OCTET STRING"

checkIntegerConstr :: IntegerType -> Ber.Value -> Either String ()
checkIntegerConstr JustInteger (IntegerValue _) = return ()
checkIntegerConstr (Range o c) (IntegerValue v) = let value = (fromIntegral :: Int -> Integer) v in
                                        if value >= o && value <= c then return ()
                                        else Left "error: INTEGER out of range"
checkIntegerConstr (Enum _) (IntegerValue _) = return ()
checkIntegerConstr _ _ = Left "wrong type"

decodeValue :: EntryTree -> B.ByteString -> AbsId -> Either String Ber.Frame
decodeValue tree value absId = do
    entry <- maybe noEntry Right $ getEntry absId tree
    case entry of
        (ObjType t) -> do
                let f = (Binary.decode :: B.ByteString -> Frame) value
                validateType (syntax t) (Ber.value f)
                case syntax t of
                    (Integer i) -> checkIntegerConstr i (Ber.value f)
                    _ -> Left "not supported yet"
                return f
        _ -> Left "not supported"

