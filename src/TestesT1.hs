module TestesT1 where

import Types
import Tarefa1

-- |Testes para a Tarefa1
testCases :: [(Int,Int,Int)]
testCases  = [(24, 24, 1111111111), (25, 25, 1111111111), (25, 24, 1111111111), (24, 25, 1111111111), (27, 27, 5), (28,28, 5)]

testesT1 :: [(Int, Int, Int)]
testesT1 = testCases

printResults :: Show a => [a] -> IO ()
printResults m =
    mapM_ (\a -> putStr ("\n\n" ++ show a)) m

testGenerateMaze :: [(Int,Int,Int)] -> [Maze]
testGenerateMaze [] = []
testGenerateMaze ((a,b,c):t) = generateMaze a b c : testGenerateMaze t