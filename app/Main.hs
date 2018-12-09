module Main where

import Text.Parsec
import System.Environment
import Data.List
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy as State
import System.Console.GetOpt
import System.IO.Error
import Control.Monad.Except
import Data.Bifunctor
import Data.Bool
import Data.Maybe

import Lib
import EncoderDecoder

data Flag = MibFile String | InputValue String | Tree

options :: [OptDescr Flag]
options = [
    Option [] ["mib"] (ReqArg MibFile "MIB") "mib file path",
    Option [] ["value"] (ReqArg InputValue "VALUE") "value",
    Option ['t'] ["tree"] (NoArg Tree) "show tree"]

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

_argsMap :: Map.Map String Flag -> [Flag] -> Either String (Map.Map String Flag)
_argsMap m [] = Right m
_argsMap m (x:xs) = do
    newMap <- _updateArgsMap x m
    _argsMap newMap xs

argsMap = _argsMap Map.empty

getMibFilePath :: Map.Map String Flag -> Either String String
getMibFilePath m = maybe (Left "please provide path to MIB file") (\(MibFile s) -> Right s) $ Map.lookup "mib" m

treeFlagEnabled m = Map.member "tree" m

showTreeIfNeeded :: Map.Map String Flag -> EntryTree -> IO ()
showTreeIfNeeded m tree | treeFlagEnabled m = putStrLn $ showTree (indexTree tree)
                        | otherwise = return ()

_main :: ExceptT String IO ()
_main = do
    argv <- lift $ getArgs
    args <- liftEither $ opts argv
    argMap <- liftEither $ argsMap args
    path <- liftEither $ getMibFilePath argMap
    text <- lift $ readFile path
    result <- liftEither $ first show $ runParseMib text
    lift $ showTreeIfNeeded argMap result

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
