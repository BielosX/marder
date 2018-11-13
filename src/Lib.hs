module Lib
    ( parseObjectType,
      ObjectType(..),
      Access (..),
      Status (..),
      parseIntegerType,
      IntegerType(..),
      parseSingleEnumItem,
      skipDescription,
      Type(..),
      parseEntry,
      parseIds,
      ObjId(..),
      Entry(..),
      getFullId,
      mib2Root
    ) where

import Text.Parsec
import Text.Parsec.Char
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe

type EntryId = [ObjId]

data Access = ReadOnly | ReadWrite | WriteOnly | NotAccessible deriving (Eq, Show)

data Status = Mandatory | Optional | Obsolete deriving (Eq, Show)

data IntegerType = JustInteger | Range Integer Integer | Enum [(String, Integer)]
    deriving (Eq, Show)

data Type = Integer IntegerType | ObjectIdentifier deriving (Eq, Show)

data ObjectType = ObjectType {
    name :: String,
    syntax :: Type,
    access :: Access,
    status :: Status,
    entryId :: EntryId
} deriving (Eq, Show)

type ObjectName = String

data ObjId = NumberId Integer | CharSeq String deriving (Eq, Show)

data Entry = IdDecl String EntryId | ObjType ObjectType deriving (Eq, Show)

data IndexTreeEntry = IndexTreeEntry {
    index :: Integer,
    entryName :: String,
    children :: Map.Map Integer Entry
} deriving (Eq, Show)

type NameLookupMap = Map.Map String Entry

data EntryTree = EntryTree {
    indexTree :: IndexTreeEntry,
    nameLookup :: NameLookupMap
} deriving (Eq, Show)

type AbsId = [Integer]

mib2Root :: NameLookupMap
mib2Root = Map.fromList [
    ("mgmt", IdDecl "mgmt" (fmap NumberId [1,3,6,1,2]))
    ]

getEntryId :: Entry -> [ObjId]
getEntryId (IdDecl name eid) = eid
getEntryId (ObjType o) = entryId o

_getFullId :: EntryId -> [Integer] -> NameLookupMap -> AbsId
_getFullId entry a l = case entry of
    [] -> a
    ((NumberId i):xs) -> _getFullId xs (i:a) l
    ((CharSeq sq):xs) -> _getFullId (fromMaybe [] $ sqEntry sq) a l
    where sqEntry sq = fmap (reverse . getEntryId) $ Map.lookup sq l

getFullId e l = _getFullId (reverse e) [] l

insertEntry :: Entry -> EntryTree -> EntryTree
insertEntry entry@(IdDecl name id) tree = EntryTree (indexTree tree) (Map.insert name entry lookup)
    where lookup = nameLookup tree
insertEntry entry@(ObjType objType) tree = let n = name objType in
    EntryTree (indexTree tree) (Map.insert n entry (nameLookup tree))

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

accessField :: Parsec [Char] u Access
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

statusField :: Parsec [Char] u Status
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

parseIntegerType :: Parsec [Char] u IntegerType
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

parseEntry :: Parsec [Char] ObjectName Entry
parseEntry = do
    identifier <- skipSeparators $ many1 letter
    putState identifier
    (fmap ObjType $ try parseObjectType) <|> parseObjIdAssign

parseCharSeqId = do
    letterPrefix <- many1 letter
    rest <- many (letter <|> digit <|> char '-')
    return $ CharSeq $ letterPrefix ++ rest

parseNumberId = do
    id <- many1 digit
    return $ NumberId (read id :: Integer)

parseIdentifiers :: [ObjId] -> Parsec [Char] u [ObjId]
parseIdentifiers x = do
    skipMany space
    result <- optionMaybe (try parseCharSeqId <|> parseNumberId)
    case result of
        Nothing -> return x
        (Just y) -> parseIdentifiers (y:x)

parseIds :: Parsec [Char] u [ObjId]
parseIds = do
    result <- skipSeparators $ curlyBraces $ parseIdentifiers []
    case result of
        [] -> fail "At least one identifier should be specified"
        x -> return $ reverse x

parseObjIdAssign = do
    string "OBJECT IDENTIFIER"
    spaces
    string "::="
    spaces
    ids <- parseIds
    name <- getState
    putState []
    return $ IdDecl name ids

parseObjectType :: Parsec [Char] ObjectName ObjectType
parseObjectType = do
    string "OBJECT-TYPE"
    skipSeparators $ string "SYNTAX"
    syntax <- parseSyntax
    skipSeparators $ string "ACCESS"
    ac <- accessField
    skipSeparators $ string "STATUS"
    stat <- statusField
    Text.Parsec.optional $ try skipDescription
    skipSeparators $ string "::="
    ids <- skipSeparators $ parseIds
    objectName <- getState
    putState []
    return (ObjectType objectName syntax ac stat ids)

