module LibSpec where

import Test.Hspec
import Text.Parsec
import Data.Either
import qualified Data.Map.Strict as Map

import Lib

parsed :: Either ParseError a -> (a -> Expectation) -> Expectation
parsed (Right x) f = f x
parsed (Left x) _ = expectationFailure $ "parser failed with error: " ++ (show x)

isObjType :: (ObjectType -> Expectation) -> Entry -> Expectation
isObjType f (ObjType t) = f t
isObjType _ _ = expectationFailure "Entry is not a ObjType"

isIdDecl f (IdDecl name i) = f name i
isIdDecl _ _ = expectationFailure "Entry is not a IdDecl"

fieldShouldBe a r = \t -> a t `shouldBe` r

libSpec = do
    describe "parse entry" $ do
        let text = "ifNumber OBJECT-TYPE \n\
                        \ SYNTAX  INTEGER \n\
                        \ ACCESS  read-only \n\
                        \ STATUS  optional \n\
                        \ DESCRIPTION \n\
                        \ \"Some text \n\
                        \   more text \n\
                        \ \" \n\
                        \ INDEX { idx } \n\
                        \ ::= { interfaces 1 }"
        let withObjId = "ifNumber OBJECT-TYPE \n\
                            \ SYNTAX  OBJECT IDENTIFIER \n\
                            \ ACCESS  read-only \n\
                            \ STATUS  mandatory \n\
                            \ ::= { system 2 }"

        let withSeqOf = "ifNumber OBJECT-TYPE \n\
                            \ SYNTAX  SEQUENCE OF IfEntry \n\
                            \ ACCESS  read-only \n\
                            \ STATUS  mandatory \n\
                            \ ::= { system 2 }"

        let withEntryRef = "ifNumber OBJECT-TYPE \n\
                            \ SYNTAX  IfEntry \n\
                            \ ACCESS  read-only \n\
                            \ STATUS  mandatory \n\
                            \ ::= { system 2 }"
        let identifierDecl = "system       OBJECT IDENTIFIER ::= { mib-2 1 }"

        it "Reads object name" $ do
            let result = runParser parseEntry [] "" text
            parsed result $ isObjType $ name `fieldShouldBe` "ifNumber"

        it "Reads access field" $ do
            let result = runParser parseEntry [] "" text
            parsed result $ isObjType $ access `fieldShouldBe` ReadOnly

        it "Reads status field" $ do
            let result = runParser parseEntry [] "" text
            parsed result $ isObjType $ status `fieldShouldBe` Optional

        it "Reads integer syntax" $ do
            let result = runParser parseEntry [] "" text
            parsed result $ isObjType $ syntax `fieldShouldBe` (Integer JustInteger)

        it "Reads object id syntax" $ do
            let result = runParser parseEntry [] "" withObjId
            parsed result $ isObjType $ syntax `fieldShouldBe` ObjectIdentifier

        it "Reads object abs id" $ do
            let result = runParser parseEntry [] "" text
            parsed result $ isObjType $ entryId `fieldShouldBe` [CharSeq "interfaces", NumberId 1]

        it "Reads id decl" $ do
            let result = runParser parseEntry [] "" identifierDecl
            parsed result $ isIdDecl $ \_ l -> l `shouldContain` [CharSeq "mib-2", NumberId 1]

        it "Reads id decl name" $ do
            let result = runParser parseEntry [] "" identifierDecl
            parsed result $ isIdDecl $ \n _ -> n `shouldBe` "system"

        it "Reads sequence of syntax" $ do
            let result = runParser parseEntry [] "" withSeqOf
            parsed result $ isObjType $ syntax `fieldShouldBe` (SequenceOf "IfEntry")

        it "Reads entry reference" $ do
            let result = runParser parseEntry [] "" withEntryRef
            let exp = (EntryRefWithConstraint "IfEntry" None)
            parsed result $ isObjType $ syntax `fieldShouldBe` exp

    describe "skip description" $ do
        let desc = " DESCRIPTION \"Some text \n\
                                 \ more text\" "
        it "skip" $ do
            parse skipDescription "" desc `shouldBe` (Right ())

    describe "parse INTEGER type" $ do
        it "Reads integer range" $ do
            let text = "INTEGER (1..15)"
            (parse parseIntegerType "" text) `shouldBe` (Right $ Range 1 15)

        it "Reads just integer" $ do
            let text = " INTEGER \n ACCESS"
            (parse parseIntegerType "" text) `shouldBe` (Right $ JustInteger)

        it "Reads enum item" $ do
            let text = " other(1) "
            (parse parseSingleEnumItem "" text) `shouldBe` (Right $ ("other", 1))

        it "Reads enum" $ do
            let text = " INTEGER { \n\
                        \ ddn-x25(1), --some comment\n\
                        \ local(2) --some comment \n\
                        \ }"
            (parse parseIntegerType "" text) `shouldBe` (Right $ Enum [("ddn-x25",1), ("local", 2)])

    describe "parse IDs" $ do
        let text = "{ system ip 1 }"
        it "Parse Ids" $ do
            (parse parseIds "" text) `shouldBe` (Right [CharSeq "system", CharSeq "ip", NumberId 1])

        it "fails if no ids" $ do
            (parse parseIds "" "{ }") `shouldSatisfy` isLeft


    describe "returns full id" $ do
        it "mgmt's child id" $
            getFullId [CharSeq "mgmt", NumberId 1] mib2Root `shouldBe` [1,3,6,1,2,1]

        it "gets full id" $ do
            let mp = Map.insert "mib-2" (IdDecl "mib-2" [CharSeq "mgmt", NumberId 1]) mib2Root
            getFullId [CharSeq "mib-2", NumberId 1] mp `shouldBe` [1,3,6,1,2,1,1]

    describe "parse many entries" $ do
        it "contains identifiers" $ do
            let text = " RFC1213-MIB DEFINITIONS ::= BEGIN \n\
                        \ mib-2       OBJECT IDENTIFIER ::= { mgmt 1 } --real programmers don't use comments \n\
                        \ \n \
                        \ \n \
                        \ -- test comment \n\
                        \ \n \
                        \ test      OBJECT IDENTIFIER ::= { mib-2 1 } --the  code is obvious\n\
                        \ END"

            let result = runParser parseMib [] "" text
            let exp = ["mgmt", "mib-2", "test"]
            parsed result $ \r -> (fmap fst $ Map.toList $ nameLookup r) `shouldContain` exp


    describe "comment" $ do
        let test = "--comment\ntest"
        let test2 = "test\
                    \ \n\
                    \ \n\
                    \ \n\
                    \ --  comment \n\
                    \ \n\
                    \ \n\
                    \ testy"
        it "skip comment" $ do
            let expr = do
                    comment
                    a <- many1 letter
                    eof
                    return a
            parse expr  "" test `shouldBe` (Right "test")


        it "skip multi lines"$ do
            let  expr = do
                    a <- skipSeparators $ many1 letter
                    b <- skipSeparators $ many1 letter
                    eof
                    return (a,b)

            parse expr "" test2 `shouldBe` (Right ("test", "testy"))

    describe "index tree" $ do
        it "inserts" $ do
            let t = insertNameToIndexTree "test1" [1] indexTreeRoot
            let r = insertNameToIndexTree "test2" [1,1] t
            getNameFromIndexTree [1,1] r `shouldBe` (Just "test2")

    describe "parse sequence" $ do
        it "parse sequence" $ do
            let seq = "ifEntry ::= \n\
                        \ SEQUENCE { \n\
                        \ el \n\
                        \    INTEGER, \n\
                        \ elTwo INTEGER, \n\
                        \ elThree \n\
                        \        Counter, \n\
                        \ elF \n\
                        \        OBJECT IDENTIFIER \n\
                        \ }"

            let r = runParser parseEntry [] "" seq
            let l = Map.fromList [("el", Integer JustInteger),
                                    ("elTwo", Integer JustInteger),
                                    ("elThree", EntryRefWithConstraint "Counter" None),
                                    ("elF", ObjectIdentifier)]
            parsed r $ \s -> s `shouldBe` (Sequence "ifEntry" l)

    describe "parse OCTET STRING" $ do
        it "parse just octet string" $ do
            let t = "OCTET STRING \n ACCESS"
            (parse parseOctetString "" t) `shouldBe` (Right $ JustString)

        it "parse bound octet string" $ do
            let t = "OCTET STRING (SIZE (0..255)) \n ACCESS"
            (parse parseOctetString "" t) `shouldBe` (Right $ BoundSize 0 255)

        it "parse strict size octet string" $ do
            let t = "OCTET STRING (SIZE (4)) \n ACCESS"
            (parse parseOctetString "" t) `shouldBe` (Right $ StrictSize 4)

    describe "parse type def" $ do
        it "parse type def without optional fields" $ do
            let t = "::= OCTET STRING"
            let expected = TypeDef "newType" (OctString JustString) noTypeDefOptionals
            let result = runParser parseTypeDef "newType" "" t
            parsed result $ \x -> x `shouldBe` expected

        it "parse type def with all optional fields" $ do
            let t = "::= [APPLICATION 3] IMPLICIT INTEGER"
            let optionals = TypeDefOptionals (Just Application) (Just Implicit) (Just 3)
            let expected = TypeDef "newType" (Integer JustInteger) optionals
            let result = runParser parseTypeDef "newType" "" t
            parsed result $ \x -> x `shouldBe` expected

        it "parse only tag number" $ do
            let t = "::= [3] INTEGER"
            let optionals = TypeDefOptionals Nothing Nothing (Just 3)
            let expected = TypeDef "newType" (Integer JustInteger) optionals
            let result = runParser parseTypeDef "newType" "" t
            parsed result $ \x -> x `shouldBe` expected
