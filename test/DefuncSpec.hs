{-# LANGUAGE QuasiQuotes #-}
module DefuncSpec where

import Test.Hspec

import Uroboro.Parser(parseFile)

import UroboroTransformations.Defunc
import PrettyPrint(renderProgram)

import Str

import Data.Either(rights)
import Data.Maybe(fromJust)

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
  eval(fun(exp), env) = autogen0_extract_eval_fun___(exp, env)
  eval(var(nat), env) = lookup(env, nat)

function interpret(Exp): Val where
  interpret(exp) = eval(exp, nil())

function nil(): Env where

function autogen0_lookup_cons(Val, Env, Nat): Val where
  autogen0_lookup_cons(val, env, zero()) = val
  autogen0_lookup_cons(val, env, succ(nat)) = lookup(env, nat)

data Val where
  autogen0_extract_eval_fun___(Exp, Env): Val

data Env where
  cons(Val, Env): Env

function apply(Val, Val): Val where
  apply(autogen0_extract_eval_fun___(exp, env), val) = eval(exp, cons(val, env))

function lookup(Env, Nat): Val where
  lookup(cons(x0, x1), x2) = autogen0_lookup_cons(x0, x1, x2)

|]

test_multi_des :: String
test_multi_des = [str|

codata A where
  A.desa1(): A
  A.desa2(): B

data B where
  conb1(): B

data Nat where
  zero(): Nat
  succ(Nat): Nat

function multi(A, Nat):A where
  multi(a, zero()).desa1().desa2() = conb1()
  multi(a, succ(n)).desa1() = multi(a, n)

|]

test_multi_des_result :: String
test_multi_des_result = [str|data B where
  conb1(): B

data Nat where
  zero(): Nat
  succ(Nat): Nat

function autogen0_desa1_multi(A, Nat): A where
  autogen0_desa1_multi(a, zero()) = autogen0_extract_desa1___multi___zero_(a)
  autogen0_desa1_multi(a, succ(n)) = multi(a, n)

data A where
  multi(A, Nat): A
  autogen0_extract_desa1___multi___zero_(A): A

function desa1(A): A where
  desa1(multi(x0, x1)) = autogen0_desa1_multi(x0, x1)

function desa2(A): B where
  desa2(autogen0_extract_desa1___multi___zero_(a)) = conb1()

|]

failure :: String
failure = [str|

codata Val where
  Val.apply(Val) : Val

function nilval(): Val where

data Nat where
  zero() : Nat
  succ(Nat) : Nat

data Exp where
  app(Exp, Exp) : Exp
  fun(Exp) : Exp
  var(Nat) : Exp

function eval(Exp): Val where
  eval(app(exp1, exp2)).apply(val) = val
  eval(fun(exp)).apply(val) = val
  eval(var(nat)) = nilval()

|]

failure_result :: String
failure_result = [str|data Nat where
  zero(): Nat
  succ(Nat): Nat

data Exp where
  app(Exp, Exp): Exp
  fun(Exp): Exp
  var(Nat): Exp

function eval(Exp): Val where
  eval(app(exp1, exp2)) = autogen0_extract_eval_app___(exp1, exp2)
  eval(fun(exp)) = autogen0_extract_eval_fun_(exp)
  eval(var(nat)) = nilval()

function nilval(): Val where

data Val where
  autogen0_extract_eval_app___(Exp, Exp): Val
  autogen0_extract_eval_fun_(Exp): Val

function apply(Val, Val): Val where
  apply(autogen0_extract_eval_app___(exp1, exp2), val) = val
  apply(autogen0_extract_eval_fun_(exp), val) = val

|]

spec :: Spec
spec = do
    describe "defunc" $ do
        it "transforms eval1.uro into eval1_result.uro" $ do
            let ptOrError = parseFile "here" eval1
            let pt = (rights [ptOrError]) !! 0
            (renderProgram (fromJust (defunc pt))) `shouldBe` eval1_result
        it "transforms test_multi_des.uro into test_multi_des_result.uro" $ do
            let ptOrError = parseFile "here" test_multi_des
            let pt = (rights [ptOrError]) !! 0
            (renderProgram (fromJust (defunc pt))) `shouldBe` test_multi_des_result
        it "transforms failure.uro into failure_result.uro" $ do
            let ptOrError = parseFile "here" failure
            let pt = (rights [ptOrError]) !! 0
            (renderProgram (fromJust (defunc pt))) `shouldBe` failure_result