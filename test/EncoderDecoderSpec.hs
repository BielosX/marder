module EncoderDecoderSpec where

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
                                encodeValue tree "text" [1,3,6,1,2,1] `shouldBe` (Left "error: expected value type INTEGER")
