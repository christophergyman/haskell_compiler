module Interpreter
(
    AExp(..),
    BExp(..),
    Com (..),
    aval,
    bval,
    eval
) where

import Data.Map 
import Machine
import Prelude hiding (lookup)

--TODO Task 2.1

data AExp =
    N Int
    | V Vname
    | Plus AExp AExp
    deriving (Eq, Read, Show)

--TODO Task 2.2
aval :: AExp -> State -> Val
aval (N i) (state) = i
aval (V x) (state) = (state ! x)
aval (Plus a1 a2) (state) = ((aval a1 state) + (aval a2 state))

--Test for arithmetic expression
avalTest = print(aval (Plus (N 3) (V "x")) (fromList [("x" ,0)]))

--TODO Task 2.1
data BExp =
    Bc Bool
    | Not BExp
    | And BExp BExp
    | Less AExp AExp
    deriving (Eq, Read, Show)

--TODO Task 2.3
bval :: BExp -> State -> Bool
--BC v
bval (Bc v) (state) = v
--Not B
bval (Not b) (state) = not(bval(b)(state))
--And b1 b2
bval (And b1 b2) (state) = (bval (b1) (state)) && (bval (b2) (state)) 

-- bval (And b1 b2) (state)

--Less a1 a2
bval (Less a1 a2) (state) = (aval (a1) (state)) < (aval (a2) (state))

--Test for boolean expression
bvalTest = print (bval (Less (N 3) ( V "x" )) (fromList [( "x" ,0)]))


--TODO Task 2.1
data Com =
    Assign Vname AExp
    | Seq Com Com 
    | If BExp Com Com
    | While (BExp) Com
    | SKIP 
    deriving (Eq, Read, Show)

--TODO Task 2.4
eval :: Com -> State -> State
--Skip, does nothing
eval (SKIP) (state) = (state)
--Assign v x, assings the outcome of an arithmetic expression x to variable v
eval (Assign v x) (state) = (insert v (aval x state) state)
--Seq c1 c2, Denotes a program which first executes c1 and then c2
eval (Seq c1 c2) (state) = (eval c2 (eval c1 state))

--If b c1 c2, denotes a program which execute c1 if b evaluates to TRUE and c2 otherwise
eval (If b c1 c2) (state) = if ( (bval (Not b) (state) ) == False) then (eval c1 state) else(eval c2 state)


--  fromList [("x",10)] @=? eval (If (Less (V "x") (N 5)) (Assign "x" (N 6)) (SKIP)) (fromList [("x",10)]) 

--While b c, executes c as long as b evaluates to true
eval (While b c) (state) = if (bval b state) then (eval (While b c) (eval c (state))) else (state)


-- eval (While b c) (state) = if (bval b state) then (eval c state) else (state)
-- eval (While b c) (state) = if ((bval (Not b) (state)) == False) then (eval c state) else (state)

--Tests
ex1 = print (eval SKIP (fromList []))
ex2 = print (eval (Assign "x" (N 5)) (fromList [( "x" ,0)]))
ex3 = print (eval (Seq (Assign "x"(N 5)) (Assign "x"(N 6))) (fromList [("x" ,0)]))
ex4 = print (eval (If (Less (V "x") (N 5)) (Assign "x" (N 6)) (SKIP)) (fromList [("x" ,4)]))
ex5 = print ( eval (While (Less (V "x")(N 5)) (Assign "x" (Plus (V "x") (N 1)))) (fromList [("x" ,0)]) )
