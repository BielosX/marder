module Main where

import Text.Parsec
import System.Environment
import Data.List
import qualified Data.Map.Strict as Map

import Lib

main :: IO ()
main = do
    [name] <- getArgs
    text <- readFile name
    let result = runParser parseMib [] "" text
    case result of
        (Left e) -> putStrLn $ show e
        (Right r) -> putStrLn $ showTree (indexTree r)

getChildren (IndexTreeEntry n c) = entries c

entries = fmap snd . Map.toList

hasChildren = not . null . getChildren

nxt  = "|    "
nxtS = "     "

showEntry :: IndexTreeEntry -> [String] -> String
showEntry (IndexTreeEntry n _) prefix = (concat $ reverse prefix) ++ "--- " ++ n ++ "\n"

tailIfPossible :: [a] -> [a]
tailIfPossible [] = []
tailIfPossible (x:xs) = xs

newPrefix p children last | children && last = "|":nxtS:(tailIfPossible p)
                          | children = nxt:p
                          | last = tailIfPossible p
                          | otherwise = p

_showTree :: [[IndexTreeEntry]] -> [String] -> [String]
_showTree [] _ = []
_showTree ((x:[]):ys) p | hasChildren x = (showEntry x p):(_showTree ([(getChildren x)] ++ ys) (newPrefix p True True))
                        | otherwise = (showEntry x p):(_showTree ys (newPrefix p False True))
_showTree ((x:xs):ys) p | hasChildren x = (showEntry x p):(_showTree ([(getChildren x)] ++ [xs] ++ ys) (newPrefix p True False))
                        | otherwise = (showEntry x p):(_showTree ([xs] ++ ys) (newPrefix p False False))

showTree (IndexTreeEntry n c) = concat $ _showTree [entries c] ["|"]

