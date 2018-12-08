module Main where

import Text.Parsec
import System.Environment
import Data.List
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy as State
import System.Console.GetOpt
import System.IO.Error

import Lib

data Flag = MibFile String | InputValue String

isMibFile (MibFile _) = True
isMibFile _ = False

options :: [OptDescr Flag]
options = [
    Option [] ["mib"] (ReqArg MibFile "MIB") "mib file path"]

opts argv = case getOpt Permute options argv of
                (o, _, []) -> return o
                (_, _, errs) -> ioError $ userError $ concat errs

main :: IO ()
main = do
    argv <- getArgs
    (MibFile name) <- fmap (head . filter isMibFile) (opts argv)
    text <- readFile name
    let result = runParser parseMib [] "" text
    case result of
        (Left e) -> putStrLn $ show e
        (Right r) -> putStrLn $ showTree (indexTree r)

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
