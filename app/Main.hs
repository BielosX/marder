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

import Lib

data Flag = MibFile String | InputValue String

isMibFile (MibFile _) = True
isMibFile _ = False

options :: [OptDescr Flag]
options = [
    Option [] ["mib"] (ReqArg MibFile "MIB") "mib file path"]

opts :: [String] -> Either String [Flag]
opts argv = case getOpt Permute options argv of
                (o, _, []) -> Right o
                (_, _, errs) -> Left $ concat errs

_checkRequired :: [Flag] -> Map.Map String Bool  -> Either String ()
_checkRequired [] m | foldr (&&) True m = Right ()
                    | otherwise = Left "Please provide all required args"
_checkRequired ((MibFile _):xs) m = _checkRequired xs $ Map.update (\a -> Just True) "mib" m
_checkRequired (x:xs) m = _checkRequired xs m

checkRequired f = _checkRequired f (Map.fromList [("mib", False)])

_main :: ExceptT String IO ()
_main = do
    argv <- lift $ getArgs
    args <- liftEither $ opts argv
    liftEither $ checkRequired args
    let (MibFile name) = (head . filter isMibFile) args
    text <- lift $ readFile name
    result <- liftEither $ first show $ runParseMib text
    lift $ putStrLn $ showTree (indexTree result)

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
