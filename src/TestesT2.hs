module TestesT2 where

import Types
import Tarefa2

maze1 = [
    [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
    [Wall,Food Big,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Big,Wall],
    [Wall,Food Big,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Big,Wall],
    [Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall],
    [Empty,Empty,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Wall,Wall,Wall,Empty,Wall,Wall,Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Empty],
    [Empty,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Empty,Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Empty],
    [Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall],
    [Wall,Food Big,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Big,Food Little,Food Big,Wall],
    [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]
    ]

maze2 = [
    [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
    [Wall,Food Big,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Big,Wall],
    [Wall,Food Big,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Big,Wall],
    [Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall],
    [Empty,Empty,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Wall,Wall,Wall,Empty,Wall,Wall,Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Empty],
    [Empty,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Empty,Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Empty],
    [Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall],
    [Wall,Food Big,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Big,Wall],
    [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]
    ]

maze3 = [
    [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
    [Wall,Food Big,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Big,Wall],
    [Wall,Food Big,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Big,Wall],
    [Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall],
    [Empty,Empty,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Wall,Wall,Wall,Empty,Wall,Wall,Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Empty],
    [Empty,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Empty,Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Empty],
    [Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall],
    [Wall,Food Big,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Empty,Food Little,Food Big,Wall],
    [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]
    ]

player0 = (Pacman (PacState (0, (4,1), 1, L, 0, 1) 0 Open Normal))
player1 = (Ghost (GhoState (1, (2,12), 1, Null, 0, 1) Alive))
player2 = (Ghost (GhoState (2, (7,21), 1, Null, 0, 1) Dead))
player3 = (Pacman (PacState (3, (7,20), 1, R, 0, 1) 0 Open Mega))
player4 = (Ghost (GhoState (4, (7,21), 1, Null, 0, 1) Dead))
player5 = (Pacman (PacState (5, (2,11), 1, R, 0, 1) 0 Open Normal))
player6 = (Pacman (PacState (6, (2,11), 1, R, 0, 2) 0 Open Normal))

playerList = [player0, player1, player2, player3]
playerList2 = [player0, player1, player2, player3, player4, player5, player6]

estado1 = State maze1 playerList 1
estado2 = State maze2 playerList 1
estado3 = State maze3 playerList 1
estado4 = State maze3 playerList2 1
 
-- |Testes da Tarefa2 para os casos em baixo indicados
testCases :: [([Play], State)]
testCases = testCases1++testCases2++testCases3++testCases4++testCases5++testCases6++testCases7++testCases8

-- |Testes movimento para todas as direções com o pacman com orientação diferente da direção
testCases1 :: [([Play], State)]
testCases1  = [((Move 0 U:[]), estado1), ((Move 0 R:[]), estado1), ((Move 0 D:[]), estado1), ((Move 0 L:Move 0 R:[]),estado1)]

-- |Testes movimento para todas as direções com o pacman orientado na direção
testCases2 :: [([Play], State)]
testCases2  = [((Move 0 U:Move 0 U:[]),estado1), ((Move 0 R:Move 0 R:[]),estado1), ((Move 0 D:Move 0 D:[]),estado1), ((Move 0 L:[]),estado1)]

-- |Testes de passar os tuneis e passar os tuneis e voltar
testCases3 :: [([Play], State)]
testCases3  = [((Move 0 L:Move 0 L:[]),estado1), ((Move 0 L:Move 0 L:Move 0 R:Move 0 R:[]),estado1)]

-- |Testes comida pequena, grande, empty
testCases4 :: [([Play], State)]
testCases4  = [((Move 0 U:Move 0 U:[]),estado1), ((Move 0 U:Move 0 U:Move 0 U:[]),estado1), ((Move 0 U:Move 0 U:Move 0 U:Move 0 D:Move 0 D:[]),estado1)]

-- |Testes Comer 1 fantasma e empty, comer 1 fantasma e comida grande, comer 1 fantasma e comida pequena
testCases5 :: [([Play], State)]
testCases5  = [((Move 3 R:[]),estado1), ((Move 3 R:[]),estado2), ((Move 3 R:[]),estado3)]

-- |Testes comer 2 ou mais fantasmas e estes voltarem para o centro
testCases6 :: [([Play], State)]
testCases6  = [((Move 3 R:Move 3 R:[]),estado4)]

-- |Testes colisao com fantasmas e vida = 1 e nao mexer quando esta Dying
testCases7 :: [([Play], State)]
testCases7  = [((Move 5 R:Move 5 R:[]),estado4)]

-- |Testes colisao com fantasmas e vida > 1
testCases8 :: [([Play], State)]
testCases8  = [((Move 6 R:Move 6 R:[]),estado4)]

testesT2 :: [([Play], State)]
testesT2 = testCases

printResults2 :: Show a => [a] -> IO ()
printResults2 m =
    mapM_ (\a -> putStr ("\n\n" ++ show a)) m

testPlay:: [([Play], State)] -> [State]
testPlay [] = []
testPlay (((x:[]),state):t) = play x state : testPlay t
testPlay (((x:y), state):t) = testPlay ((y, (play x state)):t)



{- |

Maze do Estado 1, 2 e 3:
(muda a peça por baixo do fantasma para cada estado)

#########################
#o.....................o#
#o..........M..........o#
#.######.........######.#
 }#    #.### ###.#    #. 
 .#    #.#     #.#    #. 
#.######.#######.######.#
#o..................{?.o#
#########################

Maze do Estado 4:
(muda a peça por baixo do fantasma para cada estado)

#########################
#o.....................o#
#o.........{M..........o#
#.######.........######.#
 }#    #.### ###.#    #. 
 .#    #.#     #.#    #. 
#.######.#######.######.#
#o..................{?.o#
#########################
-}

