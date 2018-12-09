module Main where

import Text.Parsec
import System.Environment
import Data.List
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy as State
import qualified Data.ByteString.Lazy as B
import System.Console.GetOpt
import System.IO.Error
import Control.Monad.Except
import Data.Bifunctor
import Data.Bool
import Data.Maybe
import Data.List.Split
import Text.Printf

import Lib
import EncoderDecoder

data Flag = MibFile String | InputValue String | Tree | Oid String

options :: [OptDescr Flag]
options = [
    Option [] ["mib"] (ReqArg MibFile "MIB") "mib file path",
    Option [] ["value"] (ReqArg InputValue "VALUE") "value",
    Option ['t'] ["tree"] (NoArg Tree) "show tree",
    Option [] ["oid"] (ReqArg Oid "OID") "identifier"]

opts :: [String] -> Either String [Flag]
opts argv = case getOpt Permute options argv of
                (o, _, []) -> Right o
                (_, _, errs) -> Left $ concat errs

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

_updateArgsMap :: Flag -> Map.Map String Flag -> Either String (Map.Map String Flag)
_updateArgsMap a@(MibFile _) m | Map.member "mib" m = Left "argument MIB defined more than once"
                             | otherwise = Right $ Map.insert "mib" a m
_updateArgsMap a@(InputValue _) m | Map.member "value" m = Left "argument VALUE defined more than once"
                                  | otherwise = Right $ Map.insert "value" a m
_updateArgsMap a@(Tree) m | Map.member "tree" m = Left "TREE flag defined more than once"
                          | otherwise = Right $ Map.insert "tree" a m
_updateArgsMap a@(Oid _) m | Map.member "oid" m = Left "argument OID defined more than once"
                           | otherwise = Right $ Map.insert "oid" a m

_argsMap :: Map.Map String Flag -> [Flag] -> Either String (Map.Map String Flag)
_argsMap m [] = Right m
_argsMap m (x:xs) = do
    newMap <- _updateArgsMap x m
    _argsMap newMap xs

argsMap = _argsMap Map.empty

getMibFilePath :: Map.Map String Flag -> Either String String
getMibFilePath m = maybe (Left "please provide path to MIB file") (\(MibFile s) -> Right s) $ Map.lookup "mib" m

getOid :: Map.Map String Flag -> Maybe AbsId
getOid = fmap (fmap (read :: String -> Integer) . splitOn ".") . fmap (\(Oid oid) -> oid) . Map.lookup "oid"

getValue :: Map.Map String Flag -> Maybe String
getValue = fmap (\(InputValue v) -> v) . Map.lookup "value"

treeFlagEnabled m = Map.member "tree" m

showTreeIfNeeded :: Map.Map String Flag -> EntryTree -> IO ()
showTreeIfNeeded m tree | treeFlagEnabled m = putStrLn $ showTree (indexTree tree)
                        | otherwise = return ()

data EncodeState = None |
                   ValueProvided String |
                   OidProvided AbsId |
                   ValueNotProvided |
                   OidNotProvided |
                   OidAndValueProvided (AbsId, String)

provideValue :: Maybe String -> EncodeState -> Either String EncodeState
provideValue Nothing (OidProvided _) = Left $ "both value and oid should be provided"
provideValue Nothing _ = Right $ ValueNotProvided
provideValue (Just v) Main.None = Right $ ValueProvided v
provideValue (Just v) (ValueProvided _) = Left "value already provided"
provideValue (Just v) (OidProvided o) = Right $ OidAndValueProvided (o, v)
provideValue (Just v) ValueNotProvided = Right $ ValueProvided v
provideValue (Just v) OidNotProvided = Left $ "both value and oid should be provided"
provideValue (Just v) _ = Left "error"

provideOid :: Maybe AbsId -> EncodeState -> Either String EncodeState
provideOid Nothing (ValueProvided _) = Left $ "both value and oid should be provided"
provideOid Nothing _ = Right $ OidNotProvided
provideOid (Just v) Main.None = Right $ OidProvided v
provideOid (Just v) (ValueProvided value) = Right $ OidAndValueProvided (v, value)
provideOid (Just v) (OidProvided _) = Left "oid already provided"
provideOid (Just v) OidNotProvided = Right $ OidProvided v
provideOid (Just v) ValueNotProvided = Left $ "both value and oid should be provided"
provideOid (Just v) _ = Left "error"

doEncode :: EntryTree -> EncodeState -> ExceptT String IO ()
doEncode t (OidAndValueProvided (a,b)) = do
    value <- liftEither $ encodeValue t b a
    lift $ putStrLn $ unwords $ fmap (printf "0x%02X") $ B.unpack value
doEncode t _ = liftEither $ Left "wrong state"

encodeValueIfNeeded :: Map.Map String Flag ->  EntryTree -> ExceptT String IO ()
encodeValueIfNeeded m t =  do
    let v = getValue m
    s1 <- liftEither $ provideValue v Main.None
    let o = getOid m
    s2 <- liftEither $ provideOid o s1
    doEncode t s2

_main :: ExceptT String IO ()
_main = do
    argv <- lift $ getArgs
    args <- liftEither $ opts argv
    argMap <- liftEither $ argsMap args
    path <- liftEither $ getMibFilePath argMap
    text <- lift $ readFile path
    result <- liftEither $ first show $ runParseMib text
    lift $ showTreeIfNeeded argMap result
    encodeValueIfNeeded argMap result

main :: IO ()
main = do
    r <- runExceptT _main
    case r of
        (Left s) -> putStrLn s
        (Right _) -> return ()

getChildren (IndexTreeEntry n c) = entries c

entries = fmap snd . Map.toList

hasChildren = not . null . getChildren

showEntry (IndexTreeEntry n _) p = (concat p) ++ "--- " ++ n ++ "\n"

sp = (take 4 $ repeat ' ') ++ "|"
li = take 5 $ repeat ' '

removeLastPrefix = reverse . drop 1 . reverse

markLast [] = []
markLast (x:[]) = [(True, x)]
markLast (x:xs) = (False, x):(markLast xs)

_showTree :: IndexTreeEntry -> Bool -> State.State [String] [String]
_showTree e@(IndexTreeEntry n c) last = do
    prefix <- get
    let entry = showEntry e prefix
    if hasChildren e then
        if last then
            put ((removeLastPrefix prefix) ++ [li] ++ [sp])
        else put (prefix ++ [sp])
    else put prefix
    let children = markLast $ getChildren e
    c <- mapM (\(a,b) -> _showTree b a) children
    if last then
        put (removeLastPrefix prefix)
    else put prefix
    return ([entry] ++ (concat c))

showTree e = concat $ fst $ runState (_showTree e True) ["|"]
