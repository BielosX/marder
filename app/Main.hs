module Main where

import Text.Parsec
import System.Environment

import Lib

main :: IO ()
main = do
    [name] <- getArgs
    text <- readFile name
    let result = runParser parseMib [] "" text
    putStrLn $ show result
