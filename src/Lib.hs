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
      mib2Root,
      parseMib,
      EntryTree(..),
      comment,
      skipSeparators,
      insertNameToIndexTree,
      getNameFromIndexTree,
      indexTreeRoot,
      parseSequence,
      parseOctetString,
      OctetString(..),
      TypeConstraint(..),
      IndexTreeEntry(..)
    ) where

import Text.Parsec
import Text.Parsec.Char
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe

type EntryId = [ObjId]

data Access = ReadOnly | ReadWrite | WriteOnly | NotAccessible deriving (Eq, Show)

data Status = Mandatory |
              Optional |
              Obsolete |
              Deprecated  deriving (Eq, Show)

data IntegerType = JustInteger | Range Integer Integer | Enum [(String, Integer)]
    deriving (Eq, Show)

data OctetString = JustString |
                   BoundSize Integer Integer |
                   StrictSize Integer deriving (Eq, Show)


data Type = Integer IntegerType |
            ObjectIdentifier |
            OctString OctetString |
            EntryRefWithConstraint EntryRef TypeConstraint |
            SequenceOf EntryRef deriving (Eq, Show)

data TypeConstraint = None |
                      StringRange Integer Integer |
                      StringExact Integer |
                      IntegerRange Integer Integer deriving (Eq, Show)

type EntryRef = String

newtype SeqIndex = SeqIndex [EntryRef] deriving (Eq, Show)

data ObjectType = ObjectType {
    name :: String,
    syntax :: Type,
    access :: Access,
    status :: Status,
    entryId :: EntryId,
    index :: Maybe SeqIndex
} deriving (Eq, Show)

type ObjectName = String

data ObjId = NumberId Integer | CharSeq String deriving (Eq, Show)

data Entry = IdDecl String EntryId |
             ObjType ObjectType |
             TypeDef String Type |
             Sequence String (Map.Map String Type) deriving (Eq, Show)

data IndexTreeEntry = IndexTreeEntry {
    entryName :: String,
    children :: Map.Map Integer IndexTreeEntry
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


directory = IndexTreeEntry "directory" Map.empty
mgmt = IndexTreeEntry "mgmt" Map.empty
experimental = IndexTreeEntry "experimental" Map.empty
enterprises = IndexTreeEntry "enterprises" Map.empty
private = IndexTreeEntry "private" $ Map.fromList [(1, enterprises)]
internet = IndexTreeEntry "internet" $ Map.fromList [(1, directory),
                                                    (2, mgmt),
                                                    (3, experimental),
                                                    (4, private)]
dod = IndexTreeEntry "dod" $ Map.fromList [(1, internet)]
org = IndexTreeEntry "org" $ Map.fromList [(6, dod)]
iso = IndexTreeEntry "iso" $ Map.fromList [(3, org)]
indexTreeRoot = IndexTreeEntry "root" $ Map.fromList [(1, iso)]

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

insertNameToIndexTree :: String -> AbsId -> IndexTreeEntry -> IndexTreeEntry
insertNameToIndexTree name [] tree = tree
insertNameToIndexTree name (x:[]) (IndexTreeEntry entryName children) = IndexTreeEntry entryName (Map.insert x newEntry children)
    where newEntry = IndexTreeEntry name Map.empty
insertNameToIndexTree name (x:xs) (IndexTreeEntry entryName children) = IndexTreeEntry entryName (Map.alter f x children)
    where f (Just a) = Just $ insertNameToIndexTree name xs a
          f Nothing = Nothing

getNameFromIndexTree :: AbsId -> IndexTreeEntry -> Maybe String
getNameFromIndexTree [] (IndexTreeEntry name _) = Just name
getNameFromIndexTree (x:xs) (IndexTreeEntry _ children) = (Map.lookup x children) >>= getNameFromIndexTree xs

insertEntry :: Entry -> EntryTree -> EntryTree
insertEntry entry@(IdDecl name id) tree = EntryTree newTree (Map.insert name entry lookup)
    where lookup = nameLookup tree
          newTree = insertNameToIndexTree name (getFullId id lookup) (indexTree tree)
insertEntry entry@(ObjType objType) tree = EntryTree newTree (Map.insert n entry (nameLookup tree))
    where   n = name objType
            id = entryId objType
            lookup = nameLookup tree
            newTree = insertNameToIndexTree n (getFullId id lookup) (indexTree tree)
insertEntry entry@(Sequence name _) tree = _insertEntry name entry tree
insertEntry entry@(TypeDef name _) tree = _insertEntry name entry tree

_insertEntry name entry tree = EntryTree (indexTree tree) (Map.insert name entry (nameLookup tree))

_parseMib :: EntryTree -> Parsec [Char] ObjectName EntryTree
_parseMib tree = do
    entry <- optionMaybe $ skipSeparators $ try parseEntry
    case entry of
        Nothing -> return tree
        (Just e) -> _parseMib (insertEntry e tree)

parseMib = do
    spaces
    moduleName <- many1 (letter <|> digit <|> char '-')
    spaces
    string "DEFINITIONS"
    spaces
    string "::="
    spaces
    between (string "BEGIN") (string "END") $ do
         Text.Parsec.optional $ try parseImports
         _parseMib $ EntryTree indexTreeRoot mib2Root

-- skip imports for now
parseImports = do
    skipSeparators $ between (string "IMPORTS") (char ';') $ skipMany1 (noneOf ";")

entryIdentifier = many1 (letter <|> digit <|> char '-')

comment :: Parsec [Char] u ()
comment = between (string "--") newline $ skipMany $ noneOf "\n"

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

deprecated = do
    string "deprecated"
    return Deprecated

statusField :: Parsec [Char] u Status
statusField = try mandatory <|>
              try Lib.optional <|>
              try deprecated <|>
              obsolete

nwln = do
    endOfLine
    return ()

spc = do
    space
    return ()

tabulator = do
    tab
    return ()

skipSeparators expr = do
    skipMany separator
    r <- expr
    skipMany (comment <|> nwln <|> spc <|> tabulator)
    return r

braces = between (char '(') (char ')')

curlyBraces = between (char '{') (char '}')

parseIntegerRange = do
    skipSeparators $ string "INTEGER"
    (f,s) <- parseRange
    return $ Range f s

parseSingleEnumItem = do
    id <- skipSeparators $ entryIdentifier
    val <- skipSeparators $ braces $ many1 digit
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

parseSequenceOf = do
    skipSeparators $ string "SEQUENCE OF"
    entry <- entryIdentifier
    return $ SequenceOf entry

parseEntryRefWithConstraint = do
    ref <- skipSeparators $ entryIdentifier
    c <- skipSeparators $ parseConstraint
    return $ EntryRefWithConstraint ref c

parseType = do
    (fmap Integer $ try parseIntegerType) <|>
        try parseObjectId <|>
        try parseSequenceOf <|>
        (fmap OctString $ try parseOctetString) <|>
        parseEntryRefWithConstraint

parseTypeDef = do
    skipSeparators $ string "::="
    t <- parseType
    objectName <- getState
    putState []
    return $ TypeDef objectName t

parseEntry :: Parsec [Char] ObjectName Entry
parseEntry = do
    identifier <- skipSeparators $ do
        letterPrefix <- many1 letter
        rest <- many (letter <|> digit <|> char '-')
        return $ letterPrefix ++ rest
    putState identifier
    (fmap ObjType $ try parseObjectType) <|>
        try parseObjIdAssign <|>
        try parseSequence <|>
        parseTypeDef

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

parseIndex = do
    string "INDEX"
    spaces
    ids <- skipSeparators $ curlyBraces $ sepBy1 (skipSeparators entryIdentifier) commaSep
    return $ SeqIndex ids

parseObjectType :: Parsec [Char] ObjectName ObjectType
parseObjectType = do
    string "OBJECT-TYPE"
    skipSeparators $ string "SYNTAX"
    syntax <- parseType
    skipSeparators $ string "ACCESS"
    ac <- accessField
    skipSeparators $ string "STATUS"
    stat <- skipSeparators $ statusField
    Text.Parsec.optional $ try skipDescription
    index <- optionMaybe $ try $ skipSeparators parseIndex
    skipSeparators $ string "::="
    ids <- skipSeparators $ parseIds
    objectName <- getState
    putState []
    return (ObjectType objectName syntax ac stat ids index)

parseOctetString = do
    skipSeparators $ string "OCTET STRING"
    c <- skipSeparators $ parseConstraint
    case c of
        (StringExact i) -> return $ StrictSize i
        (StringRange x y) -> return $ BoundSize x y
        None -> return JustString

parseConstraint = option None $ do
     try parseOctetStringConstraint <|>
         (fmap (\(a,b) -> IntegerRange a b)  $ parseRange)

parseOctetStringConstraint = do
    skipSeparators $ braces $ do
        string "SIZE"
        (fmap toRange $ try $ skipSeparators $ parseRange) <|>
            (fmap StringExact $ skipSeparators $ parseOctetStringStrictValue)
    where toRange (a,b) = StringRange a b

parseOctetStringStrictValue = braces $ do
    val <- skipSeparators $ many1 digit
    let v = read val :: Integer
    return v

parseRange = do
    (f,s) <- braces $ do
            f <- skipSeparators $ many1 digit
            string ".."
            s <- skipSeparators $ many1 digit
            return (f,s)
    let fstInt = read f :: Integer
    let sndInt = read s :: Integer
    return (fstInt, sndInt)

parseSequence = do
    string "::="
    skipSeparators $ string "SEQUENCE"
    s <- skipSeparators $ curlyBraces $ sepBy1 (skipSeparators f) commaSep
    objectName <- getState
    putState []
    return $ Sequence objectName $ Map.fromList s
    where f = do
            name <- skipSeparators $ entryIdentifier
            t <- skipSeparators $ parseType
            return (name, t)

