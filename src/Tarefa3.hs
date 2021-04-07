{-|
Module      : Tarefa 3
Copyright   :   André Silva
                João Nunes

O objetivo deste módulo é a __compactação do labirinto__, facilitando a leitura do mesmo.
-}
module Tarefa3 where

import Types
import Tarefa1
import Tarefa2


-- *Compactação do labirinto
compactMaze :: Maze -- ^Labirinto
            -> Instructions -- ^Instruções da compactação do labirinto
compactMaze m =
    let fC = compactCorridor m
        sC = compactRepeat fC fC 1
    in sC

-- *Compactação por corredor
-- |Função que percorre o labirinto e cria uma matriz com todos os corredores compactados.
compactCorridor :: Maze -- ^Labirinto
                -> [[(Int,Piece)]] -- ^Matriz de tuplos com a compactação por corredor
compactCorridor [] = []
compactCorridor (h:t) = (compactCorridorAUX h 0):(compactCorridor t)

-- |Função que compacta um corredor numa lista de tuplos.
compactCorridorAUX :: Corridor -- ^Corredor
                    -> Int -- ^Variável que contém o número de peças para passar à frente no corredor, ou seja, que já estão compactadas
                    -> [(Int,Piece)] -- ^Lista de tuplos com a compactação do corredor
compactCorridorAUX [] x = []
compactCorridorAUX (h:t) 0 = [(nRow, h)] ++ (compactCorridorAUX t (nRow-1))
                    where nRow = rowInt t h 1
compactCorridorAUX (h:t) x = compactCorridorAUX t (x-1)

-- |Função que determina o número de peças seguidas iguais à peça recebida.
rowInt :: Corridor -- ^Corredor
        -> Piece -- ^Peça
        -> Int -- ^Variável que guarda o número de peças seguidas iguais
        -> Int -- ^Número de peças seguidas iguais
rowInt [] h n = n
rowInt (x:y) h n    | x == h = rowInt y h (n+1)
                    | otherwise = n

-- *Aplicação das /Instructions/
-- |Função que percorre a matriz e para cada lista de tuplos chama a função para aplicar as instruções.
compactRepeat :: [[(Int,Piece)]] -- ^Matriz de tuplos com a compactação por corredor
                -> [[(Int,Piece)]] -- ^Matriz de tuplos com a compactação por corredor
                -> Int -- ^Variável que incrementa sempre que esta função é chamada, representa o número do corredor atual
                -> Instructions -- ^Instruções da compactação do labirinto
compactRepeat l [] n = []
compactRepeat l (h:t) n = (compactRepeatAUX l h 0 n n):(compactRepeat l t (n+1))

-- |Função que percorre a matriz e para cada lista de tuplos compara com a lista de tuplos entregue.
--
--  *Se igual, então insere a /Instruction Repeat/ com o número desse corredor
--  *Se diferente, então volta a repetir a função até chegar ao próprio corredor inserindo depois a /Instruction Instruct/
compactRepeatAUX :: [[(Int,Piece)]] -- ^Matriz de tuplos com a compactação por corredor
                    -> [(Int,Piece)] -- ^Lista de tuplos com a compactação do corredor
                    -> Int -- ^Variável que incrementa sempre que esta função é chamada, representa o número do corredor para repetir
                    -> Int -- ^Variável que decrementa sempre que esta função é chamada, representa o número do corredor atual até ao corredor entregue
                    -> Int -- ^Variável que guarda o número do corredor entregue
                    -> Instruction -- ^Instrução da compactação do corredor
compactRepeatAUX lP lA x 1 m = Instruct lA
compactRepeatAUX (h:t) lA x n m | lA == h && n <= m = Repeat x
                                | otherwise = compactRepeatAUX t lA (x+1) (n-1) m