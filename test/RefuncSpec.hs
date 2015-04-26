{-# LANGUAGE QuasiQuotes #-}
module RefuncSpec where

import Test.Hspec

import Uroboro.Parser(parseFile)

import UroboroTransformations.Refunc
import PrettyPrint(renderProgram)

import Str

import Data.Either(rights)
import Data.Maybe(fromJust)

-- what follows are heredocs representing .uro files

simple_refunc_test :: String
simple_refunc_test = [str|

data Nat where
  zero():Nat
  succ(Nat):Nat

codata List where
  List.head(): Nat
  List.tail(): List

function repeat(Nat): List where
  repeat(zero()).head() = zero()
  repeat(succ(n)).head() = succ(n)
  repeat(n).tail() = repeat(n)

|]

simple_refunc_test_result :: String
simple_refunc_test_result = [str|codata List where
  List.head(): Nat
  List.tail(): List

function repeat(Nat): List where
  repeat(zero).head() = zero.autogen0_extract_head___repeat_()
  repeat(n).tail() = repeat(n)

codata Nat where
  Nat.autogen0_extract_head___repeat_(): Nat

function zero(): Nat where
  zero().autogen0_extract_head___repeat_() = zero()

function succ(Nat): Nat where
  succ(n).autogen0_extract_head___repeat_() = succ(n)

|]

spec :: Spec
spec = do
    describe "refunc" $ do
        it "transforms simple_refunc_test.uro into simple_refunc_test_result.uro" $ do
            let ptOrError = parseFile "here" simple_refunc_test
            let pt = (rights [ptOrError]) !! 0
            (renderProgram (fromJust (refunc pt))) `shouldBe` simple_refunc_test_result