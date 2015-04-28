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
  repeat(x0).head() = x0.autogen0_extract_head___repeat_()
  repeat(n).tail() = repeat(n)

codata Nat where
  Nat.autogen0_extract_head___repeat_(): Nat

function zero(): Nat where
  zero().autogen0_extract_head___repeat_() = zero()

function succ(Nat): Nat where
  succ(n).autogen0_extract_head___repeat_() = succ(n)

|]

entangled_test :: String
entangled_test = [str|codata List where
  List.head(): Nat
  List.tail(): List

data Nat where
  zero(): Nat
  succ(Nat): Nat

function foo(Nat, Nat): Nat where
  foo(zero(), zero()) = zero()
  foo(zero(), succ(n)) = zero()
  foo(succ(n), m) = m

function bar(Nat, Nat): List where
  bar(zero(), zero()).head() = zero()
  bar(zero(), succ(n)).head() = n
  bar(zero(), m).tail() = bar(zero(), m)
  bar(succ(n), m).tail() = bar(n, m)

|]

entangled_test_result :: String
entangled_test_result = [str|codata List where
  List.head(): Nat
  List.tail(): List

function bar(Nat, Nat): List where
  bar(x0, x1).head() = x1.autogen0_extract_head___bar___(x0)
  bar(x0, x1).tail() = x0.autogen0_extract_tail___bar___(x1)

codata Nat where
  Nat.foo(Nat): Nat
  Nat.autogen0_extract_foo_zero___(): Nat
  Nat.autogen0_extract_head___bar___(Nat): Nat
  Nat.autogen0_extract_head___bar___succ_(Nat): Nat
  Nat.autogen0_extract_head___bar___zero_(): Nat
  Nat.autogen0_extract_tail___bar___(Nat): List

function zero(): Nat where
  zero().foo(x0) = x0.autogen0_extract_foo_zero___()
  zero().autogen0_extract_foo_zero___() = zero()
  zero().autogen0_extract_head___bar___(x0) = x0.autogen0_extract_head___bar___zero_()
  zero().autogen0_extract_head___bar___succ_(n) = n
  zero().autogen0_extract_head___bar___zero_() = zero()
  zero().autogen0_extract_tail___bar___(m) = bar(zero(), m)

function succ(Nat): Nat where
  succ(n).foo(m) = m
  succ(n).autogen0_extract_foo_zero___() = zero()
  succ(x1).autogen0_extract_head___bar___(x0) = x0.autogen0_extract_head___bar___succ_(x1)
  succ(n).autogen0_extract_tail___bar___(m) = bar(n, m)

|]

spec :: Spec
spec = do
    describe "refunc" $ do
        it "transforms simple_refunc_test.uro into simple_refunc_test_result.uro" $ do
            let ptOrError = parseFile "here" simple_refunc_test
            let pt = (rights [ptOrError]) !! 0
            (renderProgram (fromJust (refunc pt))) `shouldBe` simple_refunc_test_result
        it "transforms entangled_test.uro into entangled_test_result.uro" $ do
            let ptOrError = parseFile "here" entangled_test
            let pt = (rights [ptOrError]) !! 0
            (renderProgram (fromJust (refunc pt))) `shouldBe` entangled_test_result