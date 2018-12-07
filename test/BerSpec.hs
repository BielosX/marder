module BerSpec where

import Test.Hspec

import qualified Data.ByteString.Lazy as B
import Data.Binary

import Ber

berSpec = do
            describe "encode" $ do
                it "should encode primitive integer type" $ do
                    let v = IntegerValue 5
                    (B.unpack $ encode v) `shouldBe` [1, 5]
