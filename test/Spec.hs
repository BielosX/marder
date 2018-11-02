import Test.Hspec
import Text.Parsec
import Data.Either

import Lib

main :: IO ()
main = hspec $ do
    describe "parse OBJECT-TYPE macro" $ do
        let text = "ifNumber OBJECT-TYPE \n\
                        \ SYNTAX  INTEGER \n\
                        \ ACCESS  read-only \n\
                        \ STATUS  mandatory \n\
                        \ ::= { interfaces 1 }"
        it "Reads object name" $ do
            fmap name (parse parseObjectType "" text) `shouldBe` (Right "ifNumber")

        it "Reads access field" $ do
            fmap access (parse parseObjectType "" text) `shouldBe` (Right ReadOnly)
