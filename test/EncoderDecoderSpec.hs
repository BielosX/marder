module EncoderDecoderSpec where

import qualified Data.ByteString.Lazy as B
import Test.Hspec
import Data.Either

import EncoderDecoder
import Lib

template x = "RFC1213-MIB DEFINITIONS ::= BEGIN \n" ++ x
                ++ "END"


object t = "ifNumber OBJECT-TYPE \n" ++
            "SYNTAX " ++ t ++ "\n" ++
            "ACCESS  read-only \n\
            \ STATUS  optional \n\
            \ ::= { mgmt 1 }"

getRight :: (Show a) => Either a b -> b
getRight (Left a) = error $ "Left! " ++ (show a)
getRight (Right b) = b

encoderDecoderSpec = do
                        describe "validate value" $ do
                            it "returns Left when type check error" $ do
                                let obj = template $ object "INTEGER"
                                let tree = getRight $ runParseMib obj
                                let expected = Left "error: expected value type INTEGER"
                                encodeValue tree "text" [1,3,6,1,2,1] `shouldBe` expected

                            it "returns Left when integer is smaller" $ do
                                let obj = template $ object "INTEGER (10..15)"
                                let tree = getRight $ runParseMib obj
                                let expected = Left "error: INTEGER out of range"
                                encodeValue tree "5" [1,3,6,1,2,1] `shouldBe` expected

                            it "returns Left when integer is bigger" $ do
                                let obj = template $ object "INTEGER (10..15)"
                                let tree = getRight $ runParseMib obj
                                let expected = Left "error: INTEGER out of range"
                                encodeValue tree "20" [1,3,6,1,2,1] `shouldBe` expected

                        describe "encode value" $ do
                            it "encodes enum" $ do
                                let obj = template $ object "INTEGER { one(1), two(2) }"
                                let tree = getRight $ runParseMib obj
                                let expected = Right $ B.pack [1, 1]
                                encodeValue tree "one" [1,3,6,1,2,1] `shouldBe` expected
