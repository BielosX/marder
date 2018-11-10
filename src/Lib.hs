module Lib
    ( parseObjectType,
      ObjectType(..),
      Access (..),
      Status (..),
      parseIntegerType,
      IntegerType(..),
      parseSingleEnumItem,
      skipDescription,
      Type(..)
    ) where

import Text.Parsec
import Text.Parsec.Char

data Access = ReadOnly | ReadWrite | WriteOnly | NotAccessible deriving (Eq, Show)

data Status = Mandatory | Optional | Obsolete deriving (Eq, Show)

data IntegerType = JustInteger | Range Integer Integer | Enum [(String, Integer)]
    deriving (Eq, Show)

data Type = Integer IntegerType | ObjectIdentifier deriving (Eq, Show)

data ObjectType = ObjectType {
    name :: String,
    syntax :: Type,
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

braces = between (char '(') (char ')')

curlyBraces = between (char '{') (char '}')

parseIntegerRange = do
    skipSeparators $ string "INTEGER"
    (f,s) <- braces $ do
            f <- skipSeparators $ many1 digit
            string ".."
            s <- skipSeparators $ many1 digit
            return (f,s)
    let fstInt = read f :: Integer
    let sndInt = read s :: Integer
    return $ Range fstInt sndInt

parseSingleEnumItem = do
    id <- skipSeparators $ many1 letter
    val <- braces $ many1 digit
    skipMany separator
    let int = read val :: Integer
    return (id, int)

commaSep = skipSeparators $ char ','

parseEnum = do
    skipSeparators $ string "INTEGER"
    result <- curlyBraces $ sepBy1 parseSingleEnumItem commaSep
    return $ Enum result

parseJustInteger = do
    skipSeparators $ string "INTEGER"
    return JustInteger

parseIntegerType :: Parsec [Char] () IntegerType
parseIntegerType = do
    try parseIntegerRange <|> try parseEnum <|> parseJustInteger

skipDescription = do
    skipSeparators $ string "DESCRIPTION"
    between (char '"') (char '"') $ skipMany $ noneOf ['"']

parseObjectId = do
    skipSeparators $ string "OBJECT IDENTIFIER"
    return ObjectIdentifier

parseSyntax = do
    (fmap Integer $ try parseIntegerType) <|> parseObjectId

parseObjectType :: Parsec [Char] () ObjectType
parseObjectType = do
    objectName <- skipSeparators $ many1 letter
    string "OBJECT-TYPE"
    skipSeparators $ string "SYNTAX"
    syntax <- parseSyntax
    skipSeparators $ string "ACCESS"
    ac <- accessField
    skipSeparators $ string "STATUS"
    stat <- statusField
    Text.Parsec.optional $ try skipDescription
    return (ObjectType objectName syntax ac stat)
