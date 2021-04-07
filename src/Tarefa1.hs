{-|
Module      : Tarefa 1
Copyright   :   André Silva
                João Nunes

O objetivo deste módulo é a __geração de um labirinto__, consoante a largura, altura e semente de geração aleatória dada.
-}
module Tarefa1 where

import System.Random
import Types


-- *Geração do labirinto completo
-- |Função geradora de um labirinto.
generateMaze :: Int -- ^Largura do labirinto
            -> Int  -- ^Altura do labirinto
            -> Int  -- ^Semente para a geração aleatória
            -> Maze -- ^Labirinto final
generateMaze l a s = 
    let np = numeroPecas l a
        randoms = generateRandoms np s
        labd = dividirListas (l-2) randoms
        lab = trocarPecas labd
        labp = adicionarParedes lab
        labt = inserirVazio labp a
        labc = casaFantasmas labt l a
    in labc

-- *Construção do labirinto inicial
-- |Função que determina o número total de peças que o labirinto vai ter.
numeroPecas :: Int -- ^Largura do labirinto
            -> Int -- ^Altura do labirinto
            -> Int -- ^Número total de peças
numeroPecas x y | (x-2) > 0 && (y-2) > 0 = (x-2) * (y-2)
                | otherwise = 0

-- |Função que gera os números aleatórios para cada coordenada do labirinto. Tornando-o assim aleatório.
generateRandoms :: Int -- ^Número total de peças
                -> Int -- ^Semente para a geração aleatória
                -> [Int] -- ^Lista com todos os números aleatórios
generateRandoms n seed = let gen = mkStdGen seed        -- Creates a random generator
                        in take n $ randomRs (0,99) gen -- Takes the first n elements from an infinite series of random numbers between 0-99

-- |Função que divide uma lista pela largura do labirinto desejado, criando assim uma matriz.
dividirListas :: Int -- ^Largura do labirinto
                -> [a] -- ^Lista com todos os números aleatórios
                -> [[a]] -- ^Matriz
dividirListas x [] = []
dividirListas x l = (take x l):(dividirListas x (drop x l))

-- |Função que troca os números aleatórios pela sua correspondência certa.
trocarPecas :: [[Int]] -- ^Matriz
                -> Maze -- ^Labirinto
trocarPecas [] = []
trocarPecas (h:t) = (trocarPecasAUX h):(trocarPecas t)

-- |Função que auxilia a troca dos números aleatórios pela sua correspondência.
--
-- Com as seguintes condições:  
--
--  * número aleatório == 3 representa /Food Big/
--  * número aleatório >= 70 e <= 99 representa /Wall/
--  * senão número aleatório representa /Food Little/
trocarPecasAUX :: [Int] -- ^Lista de números aleatórios
                    -> Corridor -- ^Corredor com as peças bem representadas
trocarPecasAUX [] = []
trocarPecasAUX (h:t)  | h == 3 = (Food Big):(trocarPecasAUX t)
                      | 70 <= h && h <= 99 = (Wall):(trocarPecasAUX t)
                      | otherwise = (Food Little):(trocarPecasAUX t)

-- *Inserção das /paredes/
-- |Função que adiciona as paredes ao labirinto.
adicionarParedes :: Maze -- ^Labirinto inicial
                    -> Maze -- ^Labirinto com as paredes à volta
adicionarParedes m =
    adicionarParedesCimaBaixo k
    where k = adicionarParedesLados m

-- |Função que adiciona as paredes no topo e na base do labirinto
adicionarParedesCimaBaixo :: Maze -- ^Labirinto
                            -> Maze -- ^Labirinto
adicionarParedesCimaBaixo (h:t) = 
    [paredeCheia x] ++ (h:t) ++ [paredeCheia x]
    where x = length h

-- |Função que adiciona as paredes no lado esquerdo e direito do labirinto
adicionarParedesLados :: Maze -- ^Labirinto
                        -> Maze -- ^Labirinto
adicionarParedesLados [] = []
adicionarParedesLados (h:t) = (Wall:h++[Wall]):(adicionarParedesLados t)

-- |Função auxiliar que cria um corredor com o tamanho da largura do labirinto, com apenas paredes.
paredeCheia :: Int -- ^Largura do labirinto
                -> Corridor -- ^Corredor com apenas paredes
paredeCheia y = replicate y Wall

-- *Inserção do /túnel/
-- |Função que chama a função certa dependendo se a altura do labirinto é par ou ímpar.
inserirVazio :: Maze -- ^Labirinto com as paredes
                -> Int  -- ^Altura do labirinto
                -> Maze -- ^Labirinto com as paredes e o túnel
inserirVazio l a    | a `mod` 2 == 0 = inserirVazioP l (a `div` 2 )
                    | otherwise = inserirVazioI l (a `div` 2)

-- |Função que adiciona o túnel nas coordenadas certas num labirinto com altura ímpar.
inserirVazioI :: Maze -- ^Labirinto
                -> Int -- ^Altura do labirinto dividida por 2
                -> Maze -- ^Labirinto
inserirVazioI (h:t) 0 = (Empty:(tail ((init h)++ [Empty]))) : t
inserirVazioI (h:t) i = h:inserirVazioI t (i-1)

-- |Função que adiciona o túnel nas coordenadas certas num labirinto com altura par.
inserirVazioP :: Maze -- ^Labirinto
                -> Int -- ^Altura do labirinto dividida por 2
                -> Maze -- ^Labirinto
inserirVazioP (h:t) 0 = (Empty:(tail ((init h)++ [Empty]))) : t
inserirVazioP (h:t) 1 = (Empty:(tail ((init h)++ [Empty]))) : inserirVazioP t 0
inserirVazioP (h:t) i = h:inserirVazioP t (i-1)

-- *Inserção da /casa dos fantasmas/
-- |Função que chama a função certa dependendo se a largura do labirinto é par ou ímpar.
casaFantasmas :: Maze -- ^Labirinto com as paredes e o túnel
                -> Int -- ^Largura do labirinto
                -> Int -- ^Altura do labirinto
                -> Maze -- ^Labirinto com as paredes, o túnel e a casa dos fantasmas
casaFantasmas l c a | c `mod` 2 == 0 = criarCasaFPar l (procurarAlturaCF a) (procurarLarguraCF c)  0
                    | otherwise = criarCasaFImpar l (procurarAlturaCF a) (procurarLarguraCF c)  0

-- |Função auxiliar que determina, dependendo se a largura do labirinto é par ou ímpar, a largura do labirinto que vai ser preciso deixar até ao início da casa dos fantasmas.
procurarLarguraCF :: Int -- ^Largura do labirinto
                    -> Int -- ^Largura do labirinto a deixar até ao início da coordenada pretendida
procurarLarguraCF c | c `mod` 2 == 0 = (((c - 8) `div` 2) - 1)
                    | otherwise = (((c - 9) `div` 2) - 1)

-- |Função auxiliar que determina a altura a deixar até ao início da casa dos fantasmas.
procurarAlturaCF :: Int -- ^Altura do labirinto
                    -> Int -- ^Altura do labirinto a deixar até ao início do corredor pretendido
procurarAlturaCF a = (((a - 3) `div` 2) - 1)

-- |Função que adiciona ao labirinto, de largura par, a casa dos fantasmas. Recorrendo a todas as funções necessárias para tal feito.
criarCasaFPar :: Maze -- ^Labirinto
                -> Int -- ^Altura do labirinto a deixar até ao início do corredor pretendido
                -> Int -- ^Largura do labirinto a deixar até ao início da coordenada pretendida
                -> Int -- ^Varíavel que começa em 0 e incrementa sempre que esta função volta a ser chamada
                -> Maze -- ^Labirinto
criarCasaFPar l pos_altura pos_largura x    | x == 0 = criarCasaFPar (insereVaziosCF l pos_altura pos_largura 10) pos_altura pos_largura (x + 1)
                                            | x == 1 = criarCasaFPar (insereCFPar l (pos_altura + 1) pos_largura 0) pos_altura pos_largura (x + 1)
                                            | x == 2 = criarCasaFPar (insereVaziosCF l (pos_altura + 4) pos_largura 10) pos_altura pos_largura (x + 1)
                                            | otherwise = l

-- |Função que adiciona ao labirinto, de largura ímpar, a casa dos fantasmas. Recorrendo a todas as funções necessárias para tal feito.
criarCasaFImpar :: Maze -- ^Labirinto
                -> Int -- ^Altura do labirinto a deixar até ao início corredor pretendido
                -> Int -- ^Largura do labirinto a deixar até ao início da coordenada pretendida
                -> Int -- ^Varíavel que começa em 0 e incrementa sempre que esta função volta a ser chamada
                -> Maze -- ^Labirinto
criarCasaFImpar l pos_altura pos_largura x  | x == 0 = criarCasaFImpar (insereVaziosCF l pos_altura pos_largura 11) pos_altura pos_largura (x + 1)
                                            | x == 1 = criarCasaFImpar (insereCFImpar l (pos_altura + 1) pos_largura 0) pos_altura pos_largura (x + 1)
                                            | x == 2 = criarCasaFImpar (insereVaziosCF l (pos_altura + 4) pos_largura 11) pos_altura pos_largura (x + 1)
                                            | otherwise = l

-- |Função que percorre o labirinto até encontrar o corredor pretendido. Chama, depois, a função certa para a inserção do corredor pretendido.
insereVaziosCF :: Maze -- ^Labirinto
                -> Int -- ^Altura do labirinto a deixar até ao início do corredor pretendido
                -> Int -- ^Largura do labirinto a deixar até ao início da coordenada pretendida
                -> Int -- ^Número de peças vazias que o corredor vai ter
                -> Maze -- ^Labirinto
insereVaziosCF (h:t) 0 pos_largura x = (insereVaziosCFAUX h pos_largura x):t
insereVaziosCF (h:t) pos_altura pos_largura x = h:insereVaziosCF t (pos_altura - 1) pos_largura x

-- |Função auxiliar que insere o corredor com o número certo de peças vazias, nas coordenadas certas.
insereVaziosCFAUX :: Corridor -- ^Corredor
                    -> Int -- ^Largura do labirinto a deixar até ao início da coordenada pretendida
                    -> Int -- ^Número de peças vazias que o corredor vai ter
                    -> Corridor -- ^Corredor com o número certo de peças vazias nas coordenadas certas
insereVaziosCFAUX l 0 x = replicate x Empty ++ drop x l
insereVaziosCFAUX (h:t) pos_largura x = h:insereVaziosCFAUX t (pos_largura - 1) x

-- |Função que chama as funções certas para a inserção dos três corredores da casa dos fantasmas pretendidos, num labirinto com largura par.
insereCFPar :: Maze -- ^Labirinto
                -> Int -- ^Altura do labirinto a deixar até ao início do corredor pretendido
                -> Int -- ^Largura do labirinto a deixar até ao início da coordenada pretendida
                -> Int -- ^Variável que gere qual dos três corredores é para ser construído
                -> Maze -- ^Labirinto
insereCFPar l pos_altura pos_largura x  | x == 0 = insereCFPar (insereCFPar1 l pos_altura pos_largura 0) (pos_altura + 1) pos_largura (x + 1)
                                        | x == 1 = insereCFPar (insereCFPar1 l pos_altura pos_largura 1) (pos_altura + 1) pos_largura (x + 1)
                                        | otherwise = insereCFPar1 l pos_altura pos_largura 2

-- |Função que percorre um labirinto até encontrar o corredor pretendido. Chama, depois, a função certa para a inserção dos três corredores pretendidos.
insereCFPar1 :: Maze -- ^Labirinto
                -> Int -- ^Altura do labirinto a deixar até ao início do corredor pretendido
                -> Int -- ^Largura do labirinto a deixar até ao início da coordenada pretendida
                -> Int -- ^Variável que gere qual dos três corredores é para ser construído
                -> Maze -- ^Labirinto
insereCFPar1 (h:t) 0 pos_largura x  | x == 0 = (insereCFPar1AUX h pos_largura 0):t
                                    | x == 1 = (insereCFPar1AUX h pos_largura 1):t
                                    | otherwise = (insereCFPar1AUX h pos_largura 2):t
insereCFPar1 (h:t) pos_altura pos_largura x = h:insereCFPar1 t (pos_altura - 1) pos_largura x

-- |Função auxiliar que percorre um corredor até encontrar a coordenada pretendida. Insere, depois, o corredor pretendido da casa dos fantasmas nas coordenadas certas. 
insereCFPar1AUX :: Corridor -- ^Corredor
                    -> Int -- ^Largura do labirinto a deixar até ao início da coordenada pretendida
                    -> Int -- ^Variável que gere qual dos três corredores é para ser construído
                    -> Corridor -- ^Corredor pretendido
insereCFPar1AUX l 0 x   | x == 0 = (replicate 1 Empty) ++ (replicate 3 Wall) ++ (replicate 2 Empty) ++ (replicate 3 Wall) ++ (replicate 1 Empty) ++ drop 10 l
                        | x == 1 = (replicate 1 Empty) ++ (replicate 1 Wall) ++ (replicate 6 Empty) ++ (replicate 1 Wall) ++ (replicate 1 Empty) ++ drop 10 l
                        | otherwise = (replicate 1 Empty) ++ (replicate 8 Wall) ++ (replicate 1 Empty) ++ drop 10 l
insereCFPar1AUX (h:t) pos_largura x = h:insereCFPar1AUX t (pos_largura - 1) x

-- |Função que chama as funções certas para a inserção das peças pretendidas da casa dos fantasmas nos corredores certos, num labirinto com largura ímpar.
insereCFImpar :: Maze -- ^Labirinto
                -> Int -- ^Altura do labirinto a deixar até ao início do corredor pretendido
                -> Int -- ^Largura do labirinto a deixar até ao início da coordenada pretendida
                -> Int -- ^Variável que gere qual dos três corredores é para ser construído
                -> Maze -- ^Labirinto
insereCFImpar l pos_altura pos_largura x    | x == 0 = insereCFImpar (insereCFImpar1 l pos_altura pos_largura 0) (pos_altura + 1) pos_largura (x + 1)
                                            | x == 1 = insereCFImpar (insereCFImpar1 l pos_altura pos_largura 1) (pos_altura + 1) pos_largura (x + 1)
                                            | otherwise = insereCFImpar1 l pos_altura pos_largura 2

-- |Função que percorre um labirinto até encontrar o corredor pretendido. Chama, depois, a função certa para a inserção das peças pretendidas nos corredores certos.
insereCFImpar1 :: Maze -- ^Labirinto
                -> Int -- ^Altura do labirinto a deixar até ao início do corredor pretendido
                -> Int -- ^Largura do labirinto a deixar até ao início da coordenada pretendida
                -> Int -- ^Variável que gere qual dos três corredores é para ser construído
                -> Maze -- ^Labirinto
insereCFImpar1 (h:t) 0 pos_largura x    | x == 0 = (insereCFImpar1AUX h pos_largura 0):t
                                        | x == 1 = (insereCFImpar1AUX h pos_largura 1):t
                                        | otherwise = (insereCFImpar1AUX h pos_largura 2):t
insereCFImpar1 (h:t) pos_altura pos_largura x = h:insereCFImpar1 t (pos_altura - 1) pos_largura x

-- |Função auxiliar que percorre um corredor até encontrar a coordenada pretendida. Insere, depois, as peças pretendidas da casa dos fantasmas nas coordenadas certas. 
insereCFImpar1AUX :: Corridor -- ^Corredor
                    -> Int -- ^Largura do labirinto a deixar até ao início da coordenada pretendida
                    -> Int -- ^Variável que gere qual dos três corredores é para ser construído
                    -> Corridor -- ^Corredor pretendido
insereCFImpar1AUX l 0 x | x == 0 = (replicate 1 Empty) ++ (replicate 3 Wall) ++ (replicate 3 Empty) ++ (replicate 3 Wall) ++ (replicate 1 Empty) ++ drop 11 l
                        | x == 1 = (replicate 1 Empty) ++ (replicate 1 Wall) ++ (replicate 7 Empty) ++ (replicate 1 Wall) ++ (replicate 1 Empty) ++ drop 11 l
                        | otherwise = (replicate 1 Empty) ++ (replicate 9 Wall) ++ (replicate 1 Empty) ++ drop 11 l
insereCFImpar1AUX (h:t) pos_largura x = h:insereCFImpar1AUX t (pos_largura - 1) x