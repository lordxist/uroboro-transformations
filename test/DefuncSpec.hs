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

spec :: Spec
spec = do
    describe "defunc" $ do
        it "transforms eval1.uro into eval1_result.uro" $ do
            (standardize $ fromJust (defunc $ parse eval1)) `shouldBe` (standardize $ parse eval1_result)
        it "transforms test_multi_des.uro into test_multi_des_result.uro" $ do
            (standardize $ fromJust (defunc $ parse test_multi_des)) `shouldBe` (standardize $ parse test_multi_des_result)
