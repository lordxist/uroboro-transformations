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

fibonacci :: String
fibonacci = [str|
-- Fibonacci stream example
-- from paper "Copatterns" (Abel et al.)
-- adapted for Uroboro

data Nat where
  zero(): Nat
  succ(Nat): Nat

codata List where
  List.head(): Nat
  List.tail(): List

codata FunOfFunOfNatsAndListsToList where
  FunOfFunOfNatsAndListsToList.apply1(FunOfNats,List,List): List

function zipWith(): FunOfFunOfNatsAndListsToList where
  zipWith().apply1(f, l1, l2).head() = f.apply2(l1.head(), l2.head())
  zipWith().apply1(f, l1, l2).tail() = zipWith().apply1(f, l1.tail(), l2.tail())

codata FunOfNats where
  FunOfNats.apply2(Nat,Nat): Nat

function add(): FunOfNats where
  add().apply2(zero(), n) = n
  add().apply2(succ(n), m) = succ(add().apply2(n, m))

function fib(): List where
  fib().head() = zero()
  fib().tail().head() = succ(zero())
  fib().tail().tail() = zipWith().apply1(add(), fib(), fib().tail())

|]

fibonacci_result :: String
fibonacci_result = [str|

codata Nat where
  Nat.autogen0_aux(Nat): Nat

function zero(): Nat where
  zero().autogen0_aux(n) = n

function succ(Nat): Nat where
  succ(n).autogen0_aux(m) = succ(add().apply2(n, m))

codata List where
  List.head(): Nat
  List.tail(): List

codata FunOfFunOfNatsAndListsToList where
  FunOfFunOfNatsAndListsToList.apply1(FunOfNats,List,List): List

function zipWith(): FunOfFunOfNatsAndListsToList where
  zipWith().apply1(f, l1, l2).head() = f.apply2(l1.head(), l2.head())
  zipWith().apply1(f, l1, l2).tail() = zipWith().apply1(f, l1.tail(), l2.tail())

codata FunOfNats where
  FunOfNats.apply2(Nat,Nat): Nat

function add(): FunOfNats where
  add().apply2(m, n) = m.autogen0_aux(n)

function fib(): List where
  fib().head() = zero()
  fib().tail().head() = succ(zero())
  fib().tail().tail() = zipWith().apply1(add(), fib(), fib().tail())

|]

compos :: String
compos = [str|
data T where
  a(T) : T
  b(T) : T
  c() : T

function f(T) : T where
  f(a(a(x))) = f(x)
  f(a(b(y))) = a(b(f(y)))
  f(a(c())) = a(c())
  f(b(z)) = b(f(z))
  f(c()) = c()
|]

compos_result :: String
compos_result = [str|
codata T where
  T.f(): T
  T.autogen0_aux(): T

function a(T): T where
  a(x).f() = x.autogen0_aux()
  a(x).autogen0_aux() = x.f()

function b(T): T where
  b(x).f() = b(x.f())
  b(x).autogen0_aux() = a(b(x.f()))

function c(): T where
  c().f() = c()
  c().autogen0_aux() = a(c())
|]

-- doesn't have copattern coverage
failure :: String
failure = [str|

data Nat where
  zero(): Nat
  succ(Nat): Nat

data Bool where
  true(): Bool
  false(): Bool

codata List where
  List.elemAt(Nat): Nat
  List.isEmpty(): Bool

function repeat(Nat): List where
  repeat(zero()).elemAt(n) = zero()
  repeat(succ(m)).elemAt(n) = succ(m)

|]

spec :: Spec
spec = do
    describe "refunc" $ do
        it "transforms simple_refunc_test.uro into simple_refunc_test_result.uro" $ do
            (standardize $ fromJust (refunc $ parse simple_refunc_test)) `shouldBe` (standardize $ parse simple_refunc_test_result)
        it "transforms entangled_test.uro into entangled_test_result.uro" $ do
            (standardize $ fromJust (refunc $ parse entangled_test)) `shouldBe` (standardize $ parse entangled_test_result)
        it "transforms compos.uro into compos_result.uro" $ do
            (standardize $ fromJust (refunc $ parse compos)) `shouldBe` (standardize $ parse compos_result)
        it "fails with Nothing when trying to transform failure.uro" $ do
            (refunc $ parse failure) `shouldBe` Nothing
        it "transforms fibonacci.uro into fibonacci_result.uro" $ do
            (standardize $ fromJust (refunc $ parse fibonacci)) `shouldBe` (standardize $ parse fibonacci_result)
