module Lib
    ( parseObjectType,
      ObjectType(..),
      Access (..),
      Status (..)
    ) where

import Text.Parsec
import Text.Parsec.Char

data Access = ReadOnly | ReadWrite | WriteOnly | NotAccessible deriving (Eq, Show)

data Status = Mandatory | Optional | Obsolete deriving (Eq, Show)

data ObjectType = ObjectType {
    name :: String,
    syntax :: String,
    access :: Access,
    status :: Status
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

mandatory = do
    string "mandatory"
    return Mandatory

optional = do
    string "optional"
    return Optional

obsolete = do
    string "obsolete"
    return Obsolete

statusField :: Parsec [Char] () Status
statusField = try mandatory <|>
              try Lib.optional <|>
              obsolete

skipSeparators expr = do
    skipMany separator
    r <- expr
    skipMany separator
    return r

parseObjectType :: Parsec [Char] () ObjectType
parseObjectType = do
    objectName <- skipSeparators $ many1 letter
    string "OBJECT-TYPE"
    skipSeparators $ string "SYNTAX"
    syntax <- many1 letter
    skipSeparators $ string "ACCESS"
    ac <- accessField
    skipSeparators $ string "STATUS"
    stat <- statusField
    return (ObjectType objectName syntax ac stat)
