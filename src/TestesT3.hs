module TestesT3 where

import Types
import Tarefa3

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
    [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
    [Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall],
    [Empty,Empty,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Wall,Wall,Wall,Empty,Wall,Wall,Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Empty],
    [Empty,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Empty,Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Empty],
    [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
    [Wall,Food Big,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Big,Food Little,Food Big,Wall],
    [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]
    ]

maze3 = [
    [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
    [Wall,Food Little,Food Little,Wall,Food Little,Food Little,Food Little,Food Little,Food Little,Wall,Food Little,Wall,Food Little,Food Little,Wall],
    [Wall,Food Little,Food Little,Food Little,Wall,Food Little,Food Little,Food Little,Food Little,Wall,Food Little,Food Little,Wall,Food Little,Wall],
    [Wall,Food Little,Food Little,Food Little,Wall,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Big,Wall],
    [Wall,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Wall,Food Little,Wall,Food Little,Food Little,Food Little,Wall],
    [Wall,Food Little,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Food Big,Wall],
    [Wall,Food Little,Empty,Wall,Wall,Wall,Empty,Empty,Empty,Wall,Wall,Wall,Empty,Food Little,Wall],
    [Empty,Food Little,Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty,Food Little,Empty],
    [Wall,Wall,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty,Food Little,Wall],
    [Wall,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Food Little,Wall],
    [Wall,Wall,Food Little,Wall,Food Little,Food Little,Food Little,Food Little,Food Little,Wall,Food Little,Wall,Food Little,Food Little,Wall],
    [Wall,Wall,Wall,Wall,Wall,Food Little,Wall,Food Little,Food Little,Food Little,Wall,Food Little,Food Little,Food Little,Wall],
    [Wall,Wall,Food Little,Food Little,Wall,Wall,Food Little,Food Little,Food Little,Wall,Wall,Food Little,Wall,Food Little,Wall],
    [Wall,Food Big,Wall,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Wall,Food Little,Food Little,Food Little,Food Little,Wall],
    [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]
    ]

maze4 = [
    [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
    [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
    [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
    [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
    [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
    [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
    [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]
    ]
    
-- |Testes da Tarefa3 para os mazes acima indicados    
testCases :: [Maze]
testCases = [maze1, maze2, maze3, maze4]

printResults3 :: Show a => [a] -> IO ()
printResults3 m =
    mapM_ (\a -> putStr ("\n\n" ++ show a)) m

testesT3 :: [Maze]
testesT3 = testCases

testCompactMaze:: [Maze] -> [Instructions]
testCompactMaze [] = []
testCompactMaze (h:t) = compactMaze h: testCompactMaze t