import Test.Hspec
import Text.Parsec
import Data.Either

import Lib

main :: IO ()
main = hspec $ do
    describe "parse entry" $ do
        let text = "ifNumber OBJECT-TYPE \n\
                        \ SYNTAX  INTEGER \n\
                        \ ACCESS  read-only \n\
                        \ STATUS  optional \n\
                        \ DESCRIPTION \n\
                        \ \"Some text \n\
                        \   more text \n\
                        \ \" \n\
                        \ ::= { interfaces 1 }"
        let withObjId = "ifNumber OBJECT-TYPE \n\
                            \ SYNTAX  OBJECT IDENTIFIER \n\
                            \ ACCESS  read-only \n\
                            \ STATUS  mandatory \n\
                            \ ::= { system 2 }"
        it "Reads object name" $ do
            fmap name (runParser parseEntry [] "" text) `shouldBe` (Right "ifNumber")

        it "Reads access field" $ do
            fmap access (runParser parseEntry [] "" text) `shouldBe` (Right ReadOnly)

        it "Reads status field" $ do
            fmap status (runParser parseEntry [] "" text) `shouldBe` (Right Optional)

        it "Reads integer syntax" $ do
            let result = fmap syntax (runParser parseEntry [] "" text)
            result `shouldBe` (Right $ Integer JustInteger)

        it "Reads object id syntax" $ do
            let result = fmap syntax (runParser parseEntry [] "" withObjId)
            result `shouldBe` (Right ObjectIdentifier)

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
                        \ other(1),\n\
                        \ local(2) }"
            (parse parseIntegerType "" text) `shouldBe` (Right $ Enum [("other",1), ("local", 2)])
