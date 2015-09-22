{-# LANGUAGE QuasiQuotes #-}
module DeRefuncSpec where

import Test.Hspec

import Uroboro.Tree

import UroboroTransformations.Defunc
import UroboroTransformations.Refunc
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

toDataFragment :: [PT] -> [PT]
toDataFragment = fromJust . (defunc . (fromJust . refunc))

toCodataFragment :: [PT] -> [PT]
toCodataFragment = fromJust . (refunc . (fromJust . defunc))

spec :: Spec
spec = do
    describe "defunc followed by refunc" $ do
        it "brings eval1 to the Data Fragment" $ do
            (standardize $ fromJust $ defunc $ fromJust $ refunc $ toDataFragment $ parse eval1) `shouldBe`
                (standardize $ toDataFragment $ parse eval1)
    describe "refunc followed by defunc" $ do
        it "brings eval1 to the Codata Fragment" $ do
            (standardize $ fromJust $ refunc $ fromJust $ defunc $ toCodataFragment $ parse eval1) `shouldBe`
                (standardize $ toCodataFragment $ parse eval1)
