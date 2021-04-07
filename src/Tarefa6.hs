{-|
Module      : Tarefa 6
Copyright   :   André Silva
                João Nunes
                

= Introdução

Esta tarefa consiste na criação de um bot capaz de jogar sem input do utilizador.


= Objetivos

O objetivo deste módulo é a realização das jogadas automáticas do /Pacman/, consoante todas as 
diferentes situações em que um determinado /Pacman/ se encontra e o estado do jogo.
As jogadas fazem com que o pacman coma comidas, mas tendo em consideração a presença de /Ghosts/ por perto.
A estratégia utilizada tem sempre como prioridade a movimentação para a /Food Big/ e depois sim para um /Food Little/.
Primeiramente o /Pacman/ vê se numa jogada com a sua orientação tem qualquer tipo de /Food/, se sim vai pra lá. Se não,
ele vai percorrer todas as outras orientações e procurar por /Food/. Se mesmo assim, não conseguir encontrar vai,
novamente, percorrer todas as orientações, mas desta vez com uma distância de 2 jogadas. Por alguma razão, se não
encontrar nenhuma comida, ele tenta apenas se movimentar para alguma posição diferente de uma parede.
Depois de definida a coordenada para onde se quer deslocar, é considerada as posições atuais dos /Ghosts/ e se tiver
um na posição onde ele quer ir, volta a procurar outra orientação.

Tendo sempre em conta que o /Pacman/ também nunca consegue entrar na Casa dos /Ghosts/.
Foi considerado também o movimento para o túnel, de forma a distanciar-se do /Ghost/, que cumpre perfeitamente.


= Discussão e Conclusão

Entendemos que esta função, apesar de não o fazer da melhor forma possivel, cria jogadas possiveis
para o pacman tendo em conta varias situações, como a necessidade de comer e a presença de /Ghosts/, e,
por isso, cumpre com os seus objetivos.
-}
module Tarefa6 where 

import Types
import Tarefa2


-- *Realização das jogadas do /Pacman/
-- |Função principal que invoca todas as outras para obter a melhor jogada do /Pacman/.
bot :: Int -- ^ID do /Pacman/
    -> State -- ^Estado do jogo
    -> Maybe Play -- ^Jogada do /Pacman/
bot id s =
    let p = findPlayer s id
        bestP = bMove (maze s) (getGhosts (playersState s)) p
    in bestP

-- |Função que realiza a melhor jogada possível do /Pacman/.
bMove :: Maze -- ^Labirinto
        -> [Player] -- ^Lista de todos os /Ghost/
        -> Player -- ^Jogador /Pacman/
        -> Maybe Play -- ^Jogada do /Pacman/
bMove m lG p    | o == L = bMoveL m lG p
                | o == R = bMoveR m lG p
                | o == U = bMoveU m lG p
                | otherwise = bMoveD m lG p
            where o = getPlayerOrientation p

-- |Função que decide a melhor jogada tendo em conta a orientação para a esquerda.
bMoveL :: Maze -- ^Labirinto
        -> [Player] -- ^Lista de todos os /Ghost/
        -> Player -- ^Jogador /Pacman/
        -> Maybe Play -- ^Jogada do /Pacman/
bMoveL (h:t) lG p   | mazeShowX (h:t) (x,y-1) == Food Little || mazeShowX (h:t) (x,y-1) == Food Big = bGhosts (h:t) lG lG p (x,y-1) L 0
                    | otherwise = bOrientation (h:t) lG p R 0
                where   (a,b) = getPlayerCoords p
                        (x,y) = (a+1,b+1)

-- |Função que decide a melhor jogada tendo em conta a orientação para a direita.
bMoveR :: Maze -- ^Labirinto
        -> [Player] -- ^Lista de todos os /Ghost/
        -> Player -- ^Jogador /Pacman/
        -> Maybe Play -- ^Jogada do /Pacman/
bMoveR (h:t) lG p   | mazeShowX (h:t) (x,y+1) == Food Little || mazeShowX (h:t) (x,y+1) == Food Big = bGhosts (h:t) lG lG p (x,y+1) R 0
                    | otherwise = bOrientation (h:t) lG p L 0
                where   (a,b) = getPlayerCoords p
                        (x,y) = (a+1,b+1)

-- |Função que decide a melhor jogada tendo em conta a orientação para cima.
bMoveU :: Maze -- ^Labirinto
        -> [Player] -- ^Lista de todos os /Ghost/
        -> Player -- ^Jogador /Pacman/
        -> Maybe Play -- ^Jogada do /Pacman/
bMoveU (h:t) lG p   | mazeShowX (h:t) (x-1,y) == Food Little || mazeShowX (h:t) (x-1,y) == Food Big = bGhosts (h:t) lG lG p (x-1,y) U 0
                    | otherwise = bOrientation (h:t) lG p D 0
                where   (a,b) = getPlayerCoords p
                        (x,y) = (a+1,b+1)

-- |Função que decide a melhor jogada tendo em conta a orientação para baixo.
bMoveD :: Maze -- ^Labirinto
        -> [Player] -- ^Lista de todos os /Ghost/
        -> Player -- ^Jogador /Pacman/
        -> Maybe Play -- ^Jogada do /Pacman/
bMoveD (h:t) lG p   | mazeShowX (h:t) (x+1,y) == Food Little || mazeShowX (h:t) (x+1,y) == Food Big = bGhosts (h:t) lG lG p (x+1,y) D 0
                    | otherwise = bOrientation (h:t) lG p U 0
                where   (a,b) = getPlayerCoords p
                        (x,y) = (a+1,b+1)

-- *Funções auxiliares para a concretização da jogada do /Pacman/
-- |Função que decide qual a nova melhor orientação para o /Pacman/.
bOrientation :: Maze -- ^Labirinto
            -> [Player] -- ^Lista de todos os /Ghost/
            -> Player -- ^Jogador /Pacman/
            -> Orientation -- ^Orientação não pretendida
            -> Int -- ^Variável auxiliar
            -> Maybe Play -- ^Jogada do /Pacman/
bOrientation m lG p nO i    | mazeShowX m (x,y-1) == Food Big && L /= nO = bGhosts m lG lG p (x,y-1) L i
                            | mazeShowX m (x,y+1) == Food Big && R /= nO = bGhosts m lG lG p (x,y+1) R i
                            | mazeShowX m (x-1,y) == Food Big && U /= nO = bGhosts m lG lG p (x-1,y) U i
                            | mazeShowX m (x+1,y) == Food Big && D /= nO = bGhosts m lG lG p (x+1,y) D i
                            | mazeShowX m (x,y-1) == Food Little && L /= nO = bGhosts m lG lG p (x,y-1) L i
                            | mazeShowX m (x,y+1) == Food Little && R /= nO = bGhosts m lG lG p (x,y+1) R i
                            | mazeShowX m (x-1,y) == Food Little && U /= nO = bGhosts m lG lG p (x-1,y) U i
                            | mazeShowX m (x+1,y) == Food Little && D /= nO = bGhosts m lG lG p (x+1,y) D i
                            | otherwise = bOrientationAUX m lG p nO i
                        where   (a,b) = getPlayerCoords p
                                (x,y) = (a+1,b+1)
                                o = getPlayerOrientation p

-- |Função que decide qual a nova melhor orientação para o /Pacman/, tendo em conta que vai ser necessário dois movimentos.
bOrientationAUX :: Maze -- ^Labirinto
                -> [Player] -- ^Lista de todos os /Ghost/
                -> Player -- ^Jogador /Pacman/
                -> Orientation -- ^Orientação não pretendida
                -> Int -- ^Variável auxiliar
                -> Maybe Play -- ^Jogada do /Pacman/
bOrientationAUX (h:t) lG p nO i | mazeShowX (h:t) (x,y-2) == Food Big && L /= nO = bGhosts (h:t) lG lG p (x,y-2) L i
                                | mazeShowX (h:t) (x,y+2) == Food Big && R /= nO = bGhosts (h:t) lG lG p (x,y+2) R i
                                | mazeShowX (h:t) (x-2,y) == Food Big && U /= nO = bGhosts (h:t) lG lG p (x-2,y) U i
                                | mazeShowX (h:t) (x+2,y) == Food Big && D /= nO = bGhosts (h:t) lG lG p (x+2,y) D i
                                | mazeShowX (h:t) (x,y-2) == Food Little && L /= nO = bGhosts (h:t) lG lG p (x,y-2) L i
                                | mazeShowX (h:t) (x,y+2) == Food Little && R /= nO = bGhosts (h:t) lG lG p (x,y+2) R i
                                | mazeShowX (h:t) (x-2,y) == Food Little && U /= nO = bGhosts (h:t) lG lG p (x-2,y) U i
                                | mazeShowX (h:t) (x+2,y) == Food Little && D /= nO = bGhosts (h:t) lG lG p (x+2,y) D i
                                | y == 1 || y == 2 = bTunnel (h:t) lG p L o i
                                | y == comp || y == comp-1 = bTunnel (h:t) lG p R o i
                                | otherwise = bFinalMove (h:t) lG p i
                            where   (a,b) = getPlayerCoords p
                                    (x,y) = (a+1,b+1)
                                    comp = length h
                                    o = getPlayerOrientation p

-- |Função que realiza a última tentativa de movimentação para o /Pacman/.
bFinalMove :: Maze -- ^Labirinto
            -> [Player] -- ^Lista de todos os /Ghost/
            -> Player -- ^Jogador /Pacman/
            -> Int -- ^Variável auxiliar
            -> Maybe Play -- ^Jogada do /Pacman/
bFinalMove m lG p 5 = Nothing
bFinalMove m lG p i | i == 1 && mazeShowX m (x,y+1) /= Wall = bGhosts m lG lG p (x,y+1) R i
                    | i == 2 && mazeShowX m (x,y-1) /= Wall = bGhosts m lG lG p (x,y-1) L i
                    | i == 3 && mazeShowX m (x+1,y) /= Wall = bGhosts m lG lG p (x+1,y) D i
                    | i == 4 && mazeShowX m (x-1,y) /= Wall = bGhosts m lG lG p (x-1,y) U i
                    | otherwise = bFinalMove m lG p (i+1)
                where   (a,b) = getPlayerCoords p
                        (x,y) = (a+1,b+1)

-- *Função para a realização do movimento do /Pacman/ para o túnel
-- |Função que realiza o movimento do pacman para o túnel.
bTunnel :: Maze -- ^Labirinto
        -> [Player] -- ^Lista de todos os /Ghost/
        -> Player -- ^Jogador /Pacman/
        -> Orientation -- ^Lado do Labirinto que o /Pacman/ está
        -> Orientation -- ^Orientação do /Pacman/
        -> Int -- ^Variável auxiliar
        -> Maybe Play -- ^Jogada do /Pacman/
-- Lado esquerdo do labirinto
bTunnel (h:t) lG p L o i    | (pi == 0 || pi == 1) && x < mT = bGhosts (h:t) lG lG p (x+1,y) D i
                            | (pi == 0 || pi == 1) && x > mT = bGhosts (h:t) lG lG p (x-1,y) U i
                            | pi == 0 && (x == mT || x == mT+1) = bGhosts (h:t) lG lG p (x,y-1) L i
                            | pi == 1 && x == mT = bGhosts (h:t) lG lG p (x,y-1) L i
                            | otherwise = bOrientation (h:t) lG p o i
                        where   (a,b) = getPlayerCoords p
                                (x,y) = (a+1,b+1)
                                pi = mazePIa (h:t)
                                mA = length (h:t)
                                mT = (mA `div` 2)+1
-- Lado direito do labirinto
bTunnel (h:t) lG p R o i    | (pi == 0 || pi == 1) && x < mT = bGhosts (h:t) lG lG p (x+1,y) D i
                            | (pi == 0 || pi == 1) && x > mT = bGhosts (h:t) lG lG p (x-1,y) U i
                            | pi == 0 && (x == mT || x == mT+1) = bGhosts (h:t) lG lG p (x,y+1) R i
                            | pi == 1 && x == mT = bGhosts (h:t) lG lG p (x,y+1) R i
                            | otherwise = bOrientation (h:t) lG p o i
                        where   (a,b) = getPlayerCoords p
                                (x,y) = (a+1,b+1)
                                pi = mazePIa (h:t)
                                mA = length (h:t)
                                mT = mA `div` 2

-- *Verificação da posição dos /Ghosts/
-- |Função que verifica se existe um /Ghost/ na posição que o /Pacman/ quer ir.
bGhosts :: Maze -- ^Labirinto
        -> [Player] -- ^Lista de todos os /Ghost/ usada para a seguinte invocação
        -> [Player] -- ^Lista de todos os /Ghost/
        -> Player -- ^Jogador /Pacman/
        -> Coords -- ^Coordenada pretendida
        -> Orientation -- ^Orientação pretendida
        -> Int -- ^Variável auxiliar
        -> Maybe Play -- ^Jogada do /Pacman/
bGhosts m lG [] p (x,y) o i = Just (Move id o)
                        where   id = getPlayerID p
bGhosts m lG (h:t) p (x,y) o i  | (gX,gY) == (x,y) = bOrientation m lG p o (i+1)
                                | otherwise = bGhosts m lG t p (x,y) o i
                            where   (aX,bY) = getPlayerCoords h
                                    (gX,gY) = (aX+1,bY+1)