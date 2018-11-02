module Lib
    ( parseObjectType,
      ObjectType(..),
      Access (..)
    ) where

import Text.Parsec
import Text.Parsec.Char

data Access = ReadOnly | ReadWrite | WriteOnly | NotAccessible deriving (Eq, Show)

data ObjectType = ObjectType {
    name :: String,
    syntax :: String,
    access :: Access
} deriving (Eq, Show)

separator = space <|> newline

readOnly = do
    string "read-only"
    return ReadOnly

readWrite = do
    string "read-write"
    return ReadWrite

writeOnly = do
    string "write-only"
    return WriteOnly

notAccessible = do
    string "not-accessible"
    return NotAccessible

accessField :: Parsec [Char] () Access
accessField = try writeOnly <|>
              try readOnly <|>
              try readWrite <|>
              notAccessible

parseObjectType :: Parsec [Char] () ObjectType
parseObjectType = do
    spaces
    objectName <- many1 letter
    spaces
    string "OBJECT-TYPE"
    skipMany separator
    string "SYNTAX"
    spaces
    syntax <- many1 letter
    skipMany separator
    string "ACCESS"
    spaces
    ac <- accessField
    return (ObjectType objectName syntax ac)
