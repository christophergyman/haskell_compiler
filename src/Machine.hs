module Machine
(      
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec
) where

import Data.Map

--My haskell directory
--C:\Users\chris\Documents\ECM2418 Haskell\Haskell Coursework\coursework

--TODO Task 1.1
type Vname = String
--TODO Task 1.2
type Val = Int
--TODO Task 1.3
type State = (Map Vname Val)

--TODO Task 1.4
data Instr
        = LOADI Val
        | LOAD Vname
        | ADD 
        | STORE Vname
        | JMP Val
        | JMPLESS Val
        | JMPGE Val
        deriving (Eq, Read, Show)

--TODO Task 1.5
type Stack = [Val]

--TODO Task 1.6
type Config = (Int, State, Stack)

--TODO Task 1.7
iexec :: Instr -> Config -> Config

--LOADI X
--Loads a value x onto stack, prgm counter increment
iexec (LOADI x) (prgmCount, state, stack) = (prgmCount + 1, state, x : stack)

--LOAD 
--Loads value of variable v to stack, increment counter
iexec (LOAD v) (prgmCount, state, stack) = (prgmCount + 1, state, state ! v : stack)

--ADD
--Adds two topmost values of stack, increment counter
iexec (ADD) (prgmCount, state, stack) = (prgmCount + 1, state, tail(tail(reverse (sum stack : stack))))

--STORE v
--Stores top of stack to v, increment counter
iexec (STORE v) (prgmCount, state, stack) = (prgmCount + 1, insert v (head stack) state, [])

--JMP i
--increment counter by i
iexec (JMP i) (prgmCount, state, stack) = (prgmCount + i + 1, state, stack)

--JMPLESS i
--Compares the two topmost values of x (the top element) and y (the element right after x ) of the top of the stack and in the case that y < x, increment the prgmCount by i
iexec (JMPLESS i) (prgmCount, state, stack) = ((if (stack!!0 > stack!!1) then (prgmCount + i) else (prgmCount + 1)), state, [])


--JMPGE i
--similar to JMPLESS but compares y>= x instead
iexec (JMPGE i) (prgmCount, state, stack) = ((if (stack!!0 <= stack!!1) then (prgmCount + i + 1) else (prgmCount + 1)), state, [])

--TODO Task 1.8
exec :: [Instr] -> Config -> Config
exec [] (counter, state, stack) = (counter, state, stack)
exec (x:xs) (counter, state, stack) = exec xs (iexec x(counter, state, stack))

--1.7 Tests
ex1 = print (iexec (LOADI 5) (0, empty, []))
ex2 = print (iexec (LOAD "v1") (0 , fromList [( "v1" ,5)] , []))
ex3 = print (iexec ADD (0, empty, [5,6]))
ex4 = print (iexec (STORE "x" ) (0, empty, [5]))
ex5 = print (iexec (JMP 5) (0 , empty , []))
ex6 = print (iexec (JMPLESS 5) (0 , empty , [5,6]))
ex7 = print (iexec (JMPGE 5) (0 , empty , [5,6]))
ex8 = print (exec [LOADI 1, LOADI 2, ADD] (0, empty, []))
ex9 = print (exec [LOADI 1, STORE "v1", LOADI 2, STORE "v2" ] (0, empty, []))

main = do ex7

