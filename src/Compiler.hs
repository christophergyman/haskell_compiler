module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Interpreter

--TODO Task 3.1
acomp :: AExp -> [Instr]
acomp (N n) = [LOADI n]
acomp (V x) = [LOAD x]
acomp (Plus a1 a2) = acomp a1 ++ acomp a2 ++ [ADD]

ex1 = print (acomp (Plus (N 5) (V "x")))

-- @ in semantics means ++

--Instr
-- data Instr
--         = LOADI Val
--         | LOAD Vname
--         | ADD 
--         | STORE Vname
--         | JMP Val
--         | JMPLESS Val
--         | JMPGE Val
--         deriving (Eq, Read, Show)

-- data AExp =
--     N Int
--     | V Vname
--     | Plus AExp AExp
--     deriving (Eq, Read, Show)

--TODO Task 3.2
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Bc v) (bool) (int) = (if v == bool then [JMP int] else [])
bcomp (Not b) (bool) (int) = (bcomp b (not bool) int)
bcomp (And b1 b2) bool int =
    let cb2 = bcomp b2 bool int
        m = if bool then length cb2 else length cb2 + int
        cb1 = bcomp b1 False m
    in cb1 ++ cb2
bcomp (Less a1 a2) bool int = acomp a1 ++ acomp a2 ++ (if bool then [JMPLESS int] else [JMPGE int])


--Tests
ex2 = print (bcomp (Bc True) True 3)
ex3 = print (bcomp (Bc False) False 3)
ex4 = print (bcomp(Bc True) False 3)
ex5 = print (bcomp (Not (Bc False)) False 3)

ex6 = print (bcomp(And (Bc True) (Bc False)) True 3)
ex7 = print (bcomp (And (Bc False) (Bc False)) True 3)
ex8 = print (bcomp (And (Bc True) (Bc False)) False 3)
ex9 = print (bcomp (And (Bc False) (Bc True)) False 3)

-- data BExp =
--     Bc Bool
--     | Not BExp
--     | And BExp BExp
--     | Less AExp AExp
--     deriving (Eq, Read, Show)


--TODO Task 3.3
ccomp :: Com -> [Instr]
ccomp SKIP = []
ccomp (Assign x a) = acomp a ++ [STORE x]
ccomp (Seq c1 c2) = ccomp c1 ++ ccomp c2
ccomp (If b c1 c2) = let cc1 = ccomp c1
                         cc2 = ccomp c2
                         cb = bcomp b False (length cc1 + 1)
                     in cb ++ cc1 ++ JMP (length cc2) : cc2
ccomp (While b c) = let cc = ccomp c
                        cb = bcomp b False (length cc + 1)
                    in cb ++ cc ++ [JMP (-(length cb + length cc + 1))]

