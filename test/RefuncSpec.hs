{-# LANGUAGE QuasiQuotes #-}
module RefuncSpec where

import Test.Hspec

import Uroboro.Parser(parseFile)
import Uroboro.Tree

import UroboroTransformations.Refunc
import UroboroTransformations.Util.ForTesting
import PrettyPrint(renderProgram)

import Str

import Data.List(sort)
import Data.Maybe(fromJust)
import Data.Set(Set, fromList)

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
simple_refunc_test_result = [str|

codata List where
  List.head(): Nat
  List.tail(): List

codata Nat where
  Nat.autogen0_aux(): Nat

function repeat(Nat): List where
  repeat(n).head() = n.autogen0_aux()
  repeat(n).tail() = repeat(n)

function zero(): Nat where
  zero().autogen0_aux() = zero()

function succ(Nat): Nat where
  succ(n).autogen0_aux() = succ(n)

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
  foo(succ(m), n) = n

function bar(Nat, Nat): List where
  bar(zero(), zero()).head() = zero()
  bar(zero(), succ(n)).head() = n
  bar(zero(), n).tail() = bar(zero(), n)
  bar(succ(m), n).head() = n
  bar(succ(m), n).tail() = bar(m, n)

|]

entangled_test_result :: String
entangled_test_result = [str|codata List where
  List.head(): Nat
  List.tail(): List

function bar(Nat, Nat): List where
  bar(m, n).head() = m.autogen2_aux(n)
  bar(m, n).tail() = m.autogen3_aux(n)

codata Nat where
  Nat.foo(Nat): Nat
  Nat.autogen0_aux(): Nat
  Nat.autogen1_aux(): Nat
  Nat.autogen2_aux(Nat): Nat
  Nat.autogen3_aux(Nat): List

function zero(): Nat where
  zero().foo(n) = n.autogen0_aux()
  zero().autogen0_aux() = zero()
  zero().autogen1_aux() = zero()
  zero().autogen2_aux(n) = n.autogen1_aux()
  zero().autogen3_aux(n) = bar(zero(), n)

function succ(Nat): Nat where
  succ(m).foo(n) = n
  succ(n).autogen0_aux() = zero()
  succ(n).autogen1_aux() = n
  succ(m).autogen2_aux(n) = n
  succ(m).autogen3_aux(n) = bar(m, n)

|]

spec :: Spec
spec = do
    describe "refunc" $ do
        it "transforms simple_refunc_test.uro into simple_refunc_test_result.uro" $ do
            (standardize $ fromJust (refunc $ parse simple_refunc_test)) `shouldBe` (standardize $ parse simple_refunc_test_result)
        it "transforms entangled_test.uro into entangled_test_result.uro" $ do
            (standardize $ fromJust (refunc $ parse entangled_test)) `shouldBe` (standardize $ parse entangled_test_result)
