{-# LANGUAGE QuasiQuotes #-}
module DefuncSpec where

import Test.Hspec

import Uroboro.Tree

import UroboroTransformations.Defunc
import UroboroTransformations.Util.ForTesting
import PrettyPrint(renderProgram)

import Str

import Data.Maybe(fromJust)
import Data.Set(fromAscList)

-- what follows are heredocs representing .uro files

eval1 :: String
eval1 = [str|

data Nat where
  zero() : Nat
  succ(Nat) : Nat

data Exp where
  app(Exp, Exp) : Exp
  fun(Exp) : Exp
  var(Nat) : Exp

codata Val where
  Val.apply(Val) : Val

codata Env where
  Env.lookup(Nat) : Val

function nil() : Env where
  nil().lookup(n) = nil().lookup(n)

function cons(Val, Env) : Env where
  cons(val, env).lookup(zero()) = val
  cons(val, env).lookup(succ(nat)) = env.lookup(nat)

function eval(Exp, Env) : Val where
  eval(app(exp1, exp2), env) = eval(exp1, env).apply(eval(exp2, env))
  eval(fun(exp), env).apply(val) = eval(exp, cons(val, env))
  eval(var(nat), env) = env.lookup(nat)

function interpret(Exp) : Val where
  interpret(exp) = eval(exp, nil())
 
|]

eval1_result :: String
eval1_result = [str|data Nat where
  zero(): Nat
  succ(Nat): Nat

data Exp where
  app(Exp, Exp): Exp
  fun(Exp): Exp
  var(Nat): Exp

function eval(Exp, Env): Val where
  eval(app(exp1, exp2), env) = apply(eval(exp1, env), eval(exp2, env))
  eval(fun(exp), env) = autogen1_aux(exp, env)
  eval(var(nat), env) = lookup(env, nat)

function interpret(Exp): Val where
  interpret(exp) = eval(exp, nil())

function autogen0_aux(Val, Env, Nat): Val where
  autogen0_aux(val, env, zero()) = val
  autogen0_aux(val, env, succ(nat)) = lookup(env, nat)

data Val where
  autogen1_aux(Exp, Env): Val

data Env where
  cons(Val, Env): Env
  nil(): Env

function apply(Val, Val): Val where
  apply(autogen1_aux(exp, env), val) = eval(exp, cons(val, env))

function lookup(Env, Nat): Val where
  lookup(nil(), n) = lookup(nil(), n)
  lookup(cons(x0, x1), x2) = autogen0_aux(x0, x1, x2)

|]

test_multi_des :: String
test_multi_des = [str|

codata N where
  N.des1(): N
  N.des2(): N

function fun(): N where
  fun().des1().des1() = fun()
  fun().des1().des2() = fun().des2()
  fun().des2() = fun().des1()

|]

test_multi_des_result :: String
test_multi_des_result = [str|data N where
  fun(): N
  autogen0_aux(): N

function des1(N): N where
  des1(fun()) = autogen0_aux()
  des1(autogen0_aux()) = fun()

function des2(N): N where
  des2(fun()) = des1(fun())
  des2(autogen0_aux()) = des2(fun())


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

data Nat where
  zero(): Nat
  succ(Nat): Nat

data List where
  autogen0_aux(FunOfNats, List, List): List
  fib(): List
  autogen2_aux(): List

function head(List): Nat where
  head(autogen0_aux(f, l1, l2)) = apply2(f, head(l1), head(l2))
  head(fib()) = zero()
  head(autogen2_aux()) = succ(zero())

function tail(List): List where
  tail(autogen0_aux(f, l1, l2)) = apply1(zipWith(), f, tail(l1), tail(l2))
  tail(fib()) = autogen2_aux()
  tail(autogen2_aux()) = apply1(zipWith(), add(), fib(), tail(fib()))

data FunOfFunOfNatsAndListsToList where
  zipWith(): FunOfFunOfNatsAndListsToList

function apply1(FunOfFunOfNatsAndListsToList, FunOfNats, List, List): List where
  apply1(zipWith(), f, l1, l2) = autogen0_aux(f, l1, l2)

data FunOfNats where
  add(): FunOfNats

function apply2(FunOfNats, Nat, Nat): Nat where
  apply2(add(), n, m) = autogen1_aux(n, m)

function autogen1_aux(Nat, Nat): Nat where
  autogen1_aux(zero(), n) = n
  autogen1_aux(succ(n), m) = succ(apply2(add(), n, m))

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
    describe "defunc" $ do
        it "transforms eval1.uro into eval1_result.uro" $ do
            (standardize $ fromJust (defunc $ parse eval1)) `shouldBe` (standardize $ parse eval1_result)
        it "transforms test_multi_des.uro into test_multi_des_result.uro" $ do
            (standardize $ fromJust (defunc $ parse test_multi_des)) `shouldBe` (standardize $ parse test_multi_des_result)
        it "transforms fibonacci.uro into fibonacci_result.uro" $ do
            (standardize $ fromJust (defunc $ parse fibonacci)) `shouldBe` (standardize $ parse fibonacci_result)
        it "fails with Nothing when trying to transform failure.uro" $ do
            (defunc $ parse failure) `shouldBe` Nothing
