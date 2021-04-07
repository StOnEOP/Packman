{-|
Module      : Tarefa 5
Copyright   :   André Silva
                João Nunes
        

= Introdução

Nesta tarefa prentende-se criar as jogadas possiveis para os /Ghosts/, em dois modos diferentes,
/Chase/ e /Scatter/.


= Objetivos

O objetivo deste módulo é a realização das jogadas automáticas dos /Ghosts/, consoante todas as
diferentes situações em que um determinado /Ghost/ se encontra e o estado do jogo.
Por exemplo, se um /Ghost/ esta em /Chase mode/, as jogadas retornadas pela função ghostPlay serão jogadas
que aproximam o /Ghost/ do /Pacman/. O inverso acontece quando estamos em /Scatter mode/".
No /Chase mode/ para a decisão da jogada do /Ghost/ foram definidos os seguintes passos, em ordem de
utilização:
            1 -> /Pacman/ na casa seguinte ao /Ghost/ para qualquer orientação;
            2 -> Posição do /Ghost/ em relação à posição do /Pacman/.
    1: Se isto for verdade, então o /Ghost/ movimenta-se para essa posição.
    2: É comparada a posição atual do /Pacman/ com a posição atual do /Ghost/ e é definida a melhor orientação
    para a movimentação do mesmo. Por exemplo, se o /Pacman/ estiver à esquerda e em baixo o /Ghost/ vai
    tentar se deslocar nessa direção, tendo em conta qual das duas orientações o /Pacman/ está mais perto. Ou
    seja, comparando a distância entre as duas coordenadas de cada um.
       Depois de definida a melhor orientação para o movimento, é dada uma lista de critérios para ver se esse
    movimento é possível ou não. Sendo essa lista a seguinte, por ordem de prioridade: Orientação pretendida
    não vai contra uma parede; (*)Nova orientação escolhida não é contrária à orientação do /Pacman/ em conjunto
    com a possibilidade de se deslocar para a orientação anteriormente escolhida e não vai contra uma parede; 
    (*)Nova orientação escolhida não vai contra uma parede.

    (*)Esta nova orientação é definida sempre por esta ordem, Esquerda - Direita - Baixo - Cima

Já no /Scatter mode/ ele faz exatamente o contrário da /Chase mode/ com os mesmos critérios.

Tendo em conta que quando a orientação é para Baixo ele nunca consegue entrar dentro da Casa dos /Ghosts/.
Sempre que um /Ghost/ está dentro da Casa dos /Ghosts/ ele como prioridade é sair da casa sempre.
A movimentação para os túneis está feita, mas infelizmente não está a funcionar muito bem, por isso ele nem
sempre se movimenta para lá.

= Discussão e Conclusão

Entendemos que esta função, apesar de não o fazer da melhor forma possivel, cria jogadas possiveis e
de acordo com o modo em que o ghost se encontra, e, por isso, cumpre com os seus objetivos.
-}
module Tarefa5 where 

import Types
import Tarefa1
import Tarefa2


-- *Realização das jogadas dos /Ghosts/
-- |Função principal que invoca todas as outras para obter todas as jogadas dos /Ghosts/.
ghostPlay :: State -- ^Estado do jogo 
            -> [Play] -- ^Lista das jogadas de todos os /Ghosts/
ghostPlay s =
    let p = findPacmanPlayer (playersState s)
        lP = modeDecider s p
    in lP

-- |Função que, dependendo do modo do /Ghost/, invoca a função do modo pretendido.
modeDecider :: State -- ^Estado do jogo
            -> Player -- ^/Pacman/
            -> [Play] -- ^Lista das jogadas de todos os /Ghosts/
modeDecider (State m [] l) p = []
modeDecider (State m ((Pacman h):t) l) p = modeDecider (State m t l) p
modeDecider (State m ((Ghost h):t) l) p | (getGhostMode (Ghost h)) == Alive = (chaseMode m (Ghost h) p):(modeDecider (State m t l) p)
                                        | otherwise = (scatterMode m (Ghost h) p):(modeDecider (State m t l) p)

-- *Funções que decidem a jogada para o modo /Chase/
-- |Função que determina o movimento do /Ghost/ se o /Pacman/ estiver numa posição a uma distância de 1.
chaseMode :: Maze -- ^Labirinto
            -> Player -- ^/Ghost/
            -> Player -- ^/Pacman/
            -> Play -- ^Jogada do /Ghost/
chaseMode m g p | (getPlayerCoords p) == (x+1,y) = Move (getPlayerID g) D
                | (getPlayerCoords p) == (x-1,y) = Move (getPlayerID g) U
                | (getPlayerCoords p) == (x,y+1) = Move (getPlayerID g) R
                | (getPlayerCoords p) == (x,y-1) = Move (getPlayerID g) L
                | otherwise = chaseModeAUX m g p
            where (x,y) = getPlayerCoords g

-- |Função que decide qual a orientação para a jogada do /Ghost/ dependendo da posição atual do /Pacman/.
chaseModeAUX :: Maze -- ^Labirinto
            -> Player -- ^/Ghost/
            -> Player -- ^/Pacman/
            -> Play -- ^Jogada do /Ghost/
chaseModeAUX (h:t) g p  | insideHouse (xI,yI) (xF,yF) (x,y) == True = mExit (h:t) g p
                        | a == x && b < y = mLeft (h:t) g p L
                        | a == x && b > y = mRight (h:t) g p R
                        | a > x && b == y = mDown (h:t) g p D
                        | a < x && b == y = mUp (h:t) g p U
                        | a > x && b < y = mDownLeft (h:t) g p
                        | a > x && b > y = mDownRight (h:t) g p
                        | a < x && b < y = mUpLeft (h:t) g p
                        | a < x && b > y = mUpRight (h:t) g p
                        | otherwise = Move (getPlayerID g) (getPlayerOrientation p)
                    where   (a,b) = getPlayerCoords p
                            (x,y) = getPlayerCoords g
                            (xI,yI) = (procurarAlturaCF (length (h:t)+2),procurarLarguraCF (length h)+2)
                            (xF,yF) = finalCoordCF (h:t) (xI,yI)

-- *Funções que decidem a jogada para o modo /Scatter/
-- |Função que determina o movimento do /Ghost/ se o /Pacman/ estiver numa posição a uma distância de 1.
scatterMode :: Maze -- ^Labirinto
            -> Player -- ^/Ghost/
            -> Player -- ^/Pacman/
            -> Play -- ^Jogada do /Ghost/
scatterMode m g p   | (getPlayerCoords p) == (x+1,y) = Move (getPlayerID g) U
                    | (getPlayerCoords p) == (x-1,y) = Move (getPlayerID g) D
                    | (getPlayerCoords p) == (x,y+1) = Move (getPlayerID g) L
                    | (getPlayerCoords p) == (x,y-1) = Move (getPlayerID g) R
                    | otherwise = scatterModeAUX m g p
                where (x,y) = getPlayerCoords g

-- |Função que decide qual a orientação para a jogada do /Ghost/ dependendo da posição atual do /Pacman/.
scatterModeAUX :: Maze -- ^Labirinto
                -> Player -- ^/Ghost/
                -> Player -- ^/Pacman/
                -> Play -- ^Jogada do /Ghost/
scatterModeAUX (h:t) g p    | insideHouse (xI,yI) (xF,yF) (x,y) == True = mExit (h:t) g p
                            | a == x && b < y = mRight (h:t) g p R
                            | a == x && b > y = mLeft (h:t) g p L
                            | a > x && b == y = mUp (h:t) g p U
                            | a < x && b == y = mDown (h:t) g p D
                            | a > x && b < y = mUpRight (h:t) g p
                            | a > x && b > y = mUpLeft (h:t) g p
                            | a < x && b < y = mDownRight (h:t) g p
                            | a < x && b > y = mDownLeft (h:t) g p
                            | otherwise = oppMove (h:t) g p
                        where   (a,b) = getPlayerCoords p
                                (x,y) = getPlayerCoords g
                                (xI,yI) = (procurarAlturaCF (length (h:t))+2,procurarLarguraCF (length h)+2)
                                (xF,yF) = finalCoordCF (h:t) (xI,yI)

-- |Função que decide qual a orientação para a jogada do /Ghost/ dependendo da orientação do /Pacman/.
oppMove :: Maze -- ^Labirinto
        -> Player -- ^/Ghost/
        -> Player -- ^/Pacman/
        -> Play -- ^Jogada do /Ghost/
oppMove m g p   | (getPlayerOrientation p) == U = mDown m g p D
                | (getPlayerOrientation p) == D = mUp m g p U
                | (getPlayerOrientation p) == L = mRight m g p R
                | otherwise = mLeft m g p L

-- *Realização da jogada do /Ghost/
-- |Função que verifica se o /Ghost/ se encontra numa posição perto do túnel, ao se mover para a esquerda, invocando as devidas funções para cada caso.
mLeft :: Maze -- ^Labirinto
        -> Player -- ^/Ghost/
        -> Player -- ^/Pacman/
        -> Orientation -- ^Orientação auxiliar
        -> Play -- ^Jogada do /Ghost/
mLeft (h:t) g p oAUX    | y == 1 || y == 2 || y == comp || y == (comp-1) = mTunnel (h:t) g p L oAUX
                        | otherwise = mAUX (h:t) g (x,y) o L oAUX
                    where   comp = length h
                            (a,b) = getPlayerCoords g
                            (x,y) = (a+1,b+1)
                            o = getPlayerOrientation p

-- |Função que verifica se o /Ghost/ se encontra numa posição perto do túnel, ao se mover para a direta, invocando as devidas funções para cada caso.
mRight :: Maze -- ^Labirinto
        -> Player -- ^/Ghost/
        -> Player -- ^/Pacman/
        -> Orientation -- ^Orientação auxiliar
        -> Play -- ^Jogada do /Ghost/
mRight (h:t) g p oAUX   | y == 1 || y == 2 || y == comp || y == (comp-1) = mTunnel (h:t) g p R oAUX
                        | otherwise = mAUX (h:t) g (x,y) o R oAUX
                    where   comp = length h
                            (a,b) = getPlayerCoords g
                            (x,y) = (a+1,b+1)
                            o = getPlayerOrientation p

-- |Função que realiza a jogada do /Ghost/ com prioridade para a movimentação para baixo.
mDown :: Maze -- ^Labirinto
        -> Player -- ^/Ghost/
        -> Player -- ^/Pacman/
        -> Orientation -- ^Orientação auxiliar
        -> Play -- ^Jogada do /Ghost/
-- Mover só para baixo
mDown (h:t) g p D   | mazeShowX (h:t) (x+1,y) /= Wall && pi == 0 && (x+1,y) /= (xE,yE) && (x+1,y) /= (xE,yE+1) = Move (getPlayerID g) D
                    | mazeShowX (h:t) (x+1,y) /= Wall && pi == 1 && (x+1,y) /= (xE,yE) && (x+1,y) /= (xE,yE+1) && (x+1,y) /= (xE,yE+2) = Move (getPlayerID g) D
                    | mazeShowX (h:t) (x,y-1) /= Wall && (o /= R || mazeShowX (h:t) (x+1,y-1) /= Wall) = Move (getPlayerID g) L
                    | mazeShowX (h:t) (x,y+1) /= Wall && (o /= L || mazeShowX (h:t) (x+1,y+1) /= Wall) = Move (getPlayerID g) R
                    | mazeShowX (h:t) (x,y-1) /= Wall = Move (getPlayerID g) L
                    | mazeShowX (h:t) (x,y+1) /= Wall = Move (getPlayerID g) R
                    | otherwise = Move (getPlayerID g) U
                where   (a,b) = getPlayerCoords g
                        (x,y) = (a+1,b+1)
                        o = getPlayerOrientation p
                        (xE,yE) = houseExit (h:t) (procurarAlturaCF (length (h:t))+2,procurarLarguraCF (length h)+2)
                        pi = mazePIc (h:t)
-- Mover para baixo ou esquerda
mDown (h:t) g p L   | mazeShowX (h:t) (x+1,y) /= Wall && pi == 0 && (x+1,y) /= (xE,yE) && (x+1,y) /= (xE,yE+1) = Move (getPlayerID g) D
                    | mazeShowX (h:t) (x+1,y) /= Wall && pi == 1 && (x+1,y) /= (xE,yE) && (x+1,y) /= (xE,yE+1) && (x+1,y) /= (xE,yE+2) = Move (getPlayerID g) D
                    | mazeShowX (h:t) (x,y-1) /= Wall = Move (getPlayerID g) L
                    | mazeShowX (h:t) (x,y+1) /= Wall && (o /= L || mazeShowX (h:t) (x+1,y+1) /= Wall) = Move (getPlayerID g) R
                    | mazeShowX (h:t) (x-1,y) /= Wall && (o /= D || mazeShowX (h:t) (x-1,y-1) /= Wall) = Move (getPlayerID g) U
                    | mazeShowX (h:t) (x,y+1) /= Wall = Move (getPlayerID g) R
                    | otherwise = Move (getPlayerID g) U
                where   (a,b) = getPlayerCoords g
                        (x,y) = (a+1,b+1)
                        o = getPlayerOrientation p
                        (xE,yE) = houseExit (h:t) (procurarAlturaCF (length (h:t))+2,procurarLarguraCF (length h)+2)
                        pi = mazePIc (h:t)
-- Mover para baixo ou direita
mDown (h:t) g p R   | mazeShowX (h:t) (x+1,y) /= Wall && pi == 0 && (x+1,y) /= (xE,yE) && (x+1,y) /= (xE,yE+1) = Move (getPlayerID g) D
                    | mazeShowX (h:t) (x+1,y) /= Wall && pi == 1 && (x+1,y) /= (xE,yE) && (x+1,y) /= (xE,yE+1) && (x+1,y) /= (xE,yE+2) = Move (getPlayerID g) D
                    | mazeShowX (h:t) (x,y+1) /= Wall = Move (getPlayerID g) R
                    | mazeShowX (h:t) (x,y-1) /= Wall && (o /= R || mazeShowX (h:t) (x+1,y-1) /= Wall) = Move (getPlayerID g) L
                    | mazeShowX (h:t) (x-1,y) /= Wall && (o /= D || mazeShowX (h:t) (x-1,y+1) /= Wall) = Move (getPlayerID g) U
                    | mazeShowX (h:t) (x,y-1) /= Wall = Move (getPlayerID g) L
                    | otherwise = Move (getPlayerID g) U
                where   (a,b) = getPlayerCoords g
                        (x,y) = (a+1,b+1)
                        o = getPlayerOrientation p
                        (xE,yE) = houseExit (h:t) (procurarAlturaCF (length (h:t))+2,procurarLarguraCF (length h)+2)
                        pi = mazePIc (h:t)

-- |Função que realiza a jogada do /Ghost/ com prioridade para a movimentação para cima.
mUp :: Maze -- ^Labirinto
        -> Player -- ^/Ghost/
        -> Player -- ^/Pacman/
        -> Orientation -- ^Orientação auxiliar
        -> Play -- ^Jogada do /Ghost/
-- Mover só para cima
mUp (h:t) g p U | mazeShowX (h:t) (x-1,y) /= Wall = Move (getPlayerID g) U
                | mazeShowX (h:t) (x,y-1) /= Wall && (o /= R || mazeShowX (h:t) (x-1,y-1) /= Wall) = Move (getPlayerID g) L
                | mazeShowX (h:t) (x,y+1) /= Wall && (o /= L || mazeShowX (h:t) (x-1,y+1) /= Wall) = Move (getPlayerID g) R
                | mazeShowX (h:t) (x,y-1) /= Wall = Move (getPlayerID g) L
                | mazeShowX (h:t) (x,y+1) /= Wall = Move (getPlayerID g) R
                | otherwise = Move (getPlayerID g) D
            where   (a,b) = getPlayerCoords g
                    (x,y) = (a+1,b+1)
                    o = getPlayerOrientation p
-- Mover para cima ou esquerda
mUp (h:t) g p L | mazeShowX (h:t) (x-1,y) /= Wall = Move (getPlayerID g) U
                | mazeShowX (h:t) (x,y-1) /= Wall = Move (getPlayerID g) L
                | mazeShowX (h:t) (x,y+1) /= Wall && (o /= L || mazeShowX (h:t) (x-1,y+1) /= Wall) = Move (getPlayerID g) R
                | mazeShowX (h:t) (x+1,y) /= Wall && (o /= U || mazeShowX (h:t) (x+1,y-1) /= Wall) = Move (getPlayerID g) D
                | mazeShowX (h:t) (x,y+1) /= Wall = Move (getPlayerID g) R
                | otherwise = Move (getPlayerID g) D
            where   (a,b) = getPlayerCoords g
                    (x,y) = (a+1,b+1)
                    o = getPlayerOrientation p
-- Mover para cima ou direita
mUp (h:t) g p R | mazeShowX (h:t) (x-1,y) /= Wall = Move (getPlayerID g) U
                | mazeShowX (h:t) (x,y+1) /= Wall = Move (getPlayerID g) R
                | mazeShowX (h:t) (x,y+1) /= Wall && (o /= R || mazeShowX (h:t) (x-1,y-1) /= Wall) = Move (getPlayerID g) L
                | mazeShowX (h:t) (x+1,y) /= Wall && (o /= U || mazeShowX (h:t) (x+1,y+1) /= Wall) = Move (getPlayerID g) D
                | mazeShowX (h:t) (x,y+1) /= Wall = Move (getPlayerID g) R
                | otherwise = Move (getPlayerID g) D
            where   (a,b) = getPlayerCoords g
                    (x,y) = (a+1,b+1)
                    o = getPlayerOrientation p

-- |Função que verifica qual a melhor orientação possível para a jogada do /Ghost/. Definindo a prioridade em movimentação para baixo ou para a esquerda.
mDownLeft :: Maze -- ^Labirinto
            -> Player -- ^/Ghost/
            -> Player -- ^/Pacman/
            -> Play -- ^Jogada do /Ghost/
mDownLeft (h:t) g p | (a == 0 || a == 1) && (x == comp-1 || x == comp-2) = mTunnel (h:t) g p R D
                    | (a - x) > (y - b) = mDown (h:t) g p L
                    | otherwise = mLeft (h:t) g p D
                where   (x,y) = getPlayerCoords g
                        (a,b) = getPlayerCoords p
                        comp = length h

-- |Função que verifica qual a melhor orientação possível para a jogada do /Ghost/. Definindo a prioridade em movimentação para baixo ou para a direita.
mDownRight :: Maze -- ^Labirinto
            -> Player -- ^/Ghost/
            -> Player -- ^/Pacman/
            -> Play -- ^Jogada do /Ghost/
mDownRight (h:t) g p    | (a == comp-1 || a == comp-2) && (x == 0 || x == 1) = mTunnel (h:t) g p L D
                        | (a - x) > (b - y) = mDown (h:t) g p R
                        | otherwise = mRight (h:t) g p D
                    where   (x,y) = getPlayerCoords g
                            (a,b) = getPlayerCoords p
                            comp = length h

-- |Função que verifica qual a melhor orientação possível para a jogada do /Ghost/. Definindo a prioridade em movimentação para cima ou para a esquerda.
mUpLeft :: Maze -- ^Labirinto
        -> Player -- ^/Ghost/
        -> Player -- ^/Pacman/
        -> Play -- ^Jogada do /Ghost/
mUpLeft (h:t) g p   | (a == 0 || a == 1) && (x == comp-1 || x == comp-2) = mTunnel (h:t) g p R U
                    | (x - a) > (y - b) = mUp (h:t) g p L
                    | otherwise = mLeft (h:t) g p U
                where   (x,y) = getPlayerCoords g
                        (a,b) = getPlayerCoords p
                        comp = length h

-- |Função que verifica qual a melhor orientação possível para a jogada do /Ghost/. Definindo a prioridade em movimentação para cima ou para a direita.
mUpRight :: Maze -- ^Labirinto
            -> Player -- ^/Ghost/
            -> Player -- ^/Pacman/
            -> Play -- ^Jogada do /Ghost/
mUpRight (h:t) g p  | (a == comp-1 || a == comp-2) && (x == 0 || x == 1) = mTunnel (h:t) g p L U
                    | (x - a) > (b - y) = mUp (h:t) g p R
                    | otherwise = mRight (h:t) g p U
                where   (x,y) = getPlayerCoords g
                        (a,b) = getPlayerCoords p
                        comp = length h

-- *Funções auxiliares para a realização da jogada do /Ghost/
-- |Função que verifica e realiza a movimentação do /Ghost/ se estiver numa posição que consegue aceder ao túnel, verificando também se vale ou não a pena esse movimento.
mTunnel :: Maze -- ^Labirinto
        -> Player -- ^/Ghost/
        -> Player -- ^/Pacman/
        -> Orientation -- ^Orientação pretendida para o movimento
        -> Orientation -- ^Orientação auxiliar
        -> Play -- ^Jogada do /Ghost/
mTunnel (h:t) g p gO oAUX   | pi == 0 && y == 1 && (x == alt || x == alt+1) && gO == L = Move (getPlayerID g) L
                            | pi == 0 && y == comp && (x == alt || x == alt+1) && gO == R = Move (getPlayerID g) R
                            | pi == 1 && y == 1 && x == alt && gO == L = Move (getPlayerID g) L
                            | pi == 1 && y == comp && (x == alt || x == alt+1) && gO == R = Move (getPlayerID g) R
                            | b <= (comp `div` 2) && o /= R && gO == L = mTunnelAUX (h:t) g p L oAUX
                            | b >= (comp `div` 2) && o /= L && gO == R = mTunnelAUX (h:t) g p R oAUX
                            | otherwise = mAUX (h:t) g (x,y) o gO oAUX
                        where   (a,b) = getPlayerCoords p
                                (gX,gY) = getPlayerCoords g
                                (x,y) = (gX+1,gY+1)
                                comp = length h
                                alt = (length (h:t)) `div` 2
                                o = getPlayerOrientation p
                                pi = mazePIa (h:t)

-- |Função que verifica se vale ou não a pena movimentar para o túnel, invocando a devida função caso não.
mTunnelAUX :: Maze -- ^Labirinto
            -> Player -- ^/Ghost/
            -> Player -- ^/Pacman/
            -> Orientation -- ^Orientação pretendida para o movimento
            -> Orientation -- ^Orientação auxiliar
            -> Play -- ^Jogada do /Ghost/
mTunnelAUX (h:t) g p gO oAUX    | pi == 0 && (x == alt || x == alt+1) && gO == L = Move (getPlayerID g) R
                                | pi == 0 && (x == alt || x == alt+1) && gO == R = Move (getPlayerID g) L
                                | pi == 1 && x == alt && gO == L = Move (getPlayerID g) R
                                | pi == 1 && x == alt && gO == R = Move (getPlayerID g) L
                                | otherwise = mAUX (h:t) g (x,y) o gO oAUX
                            where   (a,b) = getPlayerCoords g
                                    (x,y) = (a+1,b+1)
                                    o = getPlayerOrientation p
                                    alt = (length (h:t)) `div` 2
                                    pi = mazePIa (h:t)

-- |Função que realiza a jogada do /Ghost/ dependendo das prioridades de orientação dadas.
mAUX :: Maze -- ^Labirinto
    -> Player -- ^/Ghost/
    -> Coords -- ^Coordenadas do /Pacman/
    -> Orientation -- ^Orientação do /Pacman/
    -> Orientation -- ^Orientação pretendida para o movimento
    -> Orientation -- ^Orientação auxiliar
    -> Play -- ^Jogada do /Ghost/
-- Mover só para a esquerda
mAUX (h:t) g (x,y) o L L    | mazeShowX (h:t) (x,y-1) /= Wall = Move (getPlayerID g) L
                            | mazeShowX (h:t) (x+1,y) /= Wall && (o /= U || mazeShowX (h:t) (x+1,y-1) /= Wall) = Move (getPlayerID g) D
                            | mazeShowX (h:t) (x-1,y) /= Wall && (o /= D || mazeShowX (h:t) (x-1,y-1) /= Wall) = Move (getPlayerID g) U
                            | mazeShowX (h:t) (x+1,y) /= Wall = Move (getPlayerID g) D
                            | mazeShowX (h:t) (x-1,y) /= Wall = Move (getPlayerID g) U
                            | otherwise = Move (getPlayerID g) R
-- Mover só para a direita
mAUX (h:t) g (x,y) o R R    | mazeShowX (h:t) (x,y+1) /= Wall = Move (getPlayerID g) R
                            | mazeShowX (h:t) (x+1,y) /= Wall && (o /= U || mazeShowX (h:t) (x+1,y+1) /= Wall) = Move (getPlayerID g) D
                            | mazeShowX (h:t) (x-1,y) /= Wall && (o /= D || mazeShowX (h:t) (x-1,y+1) /= Wall) = Move (getPlayerID g) U
                            | mazeShowX (h:t) (x+1,y) /= Wall = Move (getPlayerID g) D
                            | mazeShowX (h:t) (x-1,y) /= Wall = Move (getPlayerID g) U
                            | otherwise = Move (getPlayerID g) L
-- Mover para a esquerda ou baixo
mAUX (h:t) g (x,y) o L D    | mazeShowX (h:t) (x,y-1) /= Wall = Move (getPlayerID g) L
                            | mazeShowX (h:t) (x+1,y) /= Wall && pi == 0 && (x+1,y) /= (xE,yE) && (x+1,y) /= (xE,yE+1) = Move (getPlayerID g) D
                            | mazeShowX (h:t) (x+1,y) /= Wall && pi == 1 && (x+1,y) /= (xE,yE) && (x+1,y) /= (xE,yE+1) && (x+1,y) /= (xE,yE+2) = Move (getPlayerID g) D
                            | mazeShowX (h:t) (x-1,y) /= Wall && (o /= D || mazeShowX (h:t) (x-1,y-1) /= Wall) = Move (getPlayerID g) U
                            | mazeShowX (h:t) (x,y+1) /= Wall && (o /= L || mazeShowX (h:t) (x+1,y+1) /= Wall) = Move (getPlayerID g) R
                            | mazeShowX (h:t) (x-1,y) /= Wall = Move (getPlayerID g) U
                            | otherwise = Move (getPlayerID g) R
                        where   (xE,yE) = houseExit (h:t) (procurarAlturaCF (length (h:t))+2,procurarLarguraCF (length h)+2)
                                pi = mazePIc (h:t)
-- Mover para a esquerda ou cima
mAUX (h:t) g (x,y) o L U    | mazeShowX (h:t) (x,y-1) /= Wall = Move (getPlayerID g) L
                            | mazeShowX (h:t) (x-1,y) /= Wall = Move (getPlayerID g) U
                            | mazeShowX (h:t) (x+1,y) /= Wall && (o /= U || mazeShowX (h:t) (x+1,y-1) /= Wall) = Move (getPlayerID g) D
                            | mazeShowX (h:t) (x,y+1) /= Wall && (o /= L || mazeShowX (h:t) (x-1,y+1) /= Wall) = Move (getPlayerID g) R
                            | mazeShowX (h:t) (x+1,y) /= Wall = Move (getPlayerID g) D
                            | otherwise = Move (getPlayerID g) R
-- Mover para a direita ou baixo
mAUX (h:t) g (x,y) o R D    | mazeShowX (h:t) (x,y+1) /= Wall = Move (getPlayerID g) R
                            | mazeShowX (h:t) (x+1,y) /= Wall && pi == 0 && (x+1,y) /= (xE,yE) && (x+1,y) /= (xE,yE+1) = Move (getPlayerID g) D
                            | mazeShowX (h:t) (x+1,y) /= Wall && pi == 1 && (x+1,y) /= (xE,yE) && (x+1,y) /= (xE,yE+1) && (x+1,y) /= (xE,yE+2) = Move (getPlayerID g) D
                            | mazeShowX (h:t) (x-1,y) /= Wall && (o /= D || mazeShowX (h:t) (x-1,y+1) /= Wall) = Move (getPlayerID g) U
                            | mazeShowX (h:t) (x,y-1) /= Wall && (o /= R || mazeShowX (h:t) (x+1,y-1) /= Wall) = Move (getPlayerID g) L
                            | mazeShowX (h:t) (x-1,y) /= Wall = Move (getPlayerID g) U
                            | otherwise = Move (getPlayerID g) L
                        where   (xE,yE) = houseExit (h:t) (procurarAlturaCF (length (h:t))+2,procurarLarguraCF (length h)+2)
                                pi = mazePIc (h:t)
-- Mover para a direita ou cima
mAUX (h:t) g (x,y) o R U    | mazeShowX (h:t) (x,y+1) /= Wall = Move (getPlayerID g) R
                            | mazeShowX (h:t) (x-1,y) /= Wall = Move (getPlayerID g) U
                            | mazeShowX (h:t) (x+1,y) /= Wall && (o /= U || mazeShowX (h:t) (x+1,y+1) /= Wall) = Move (getPlayerID g) D
                            | mazeShowX (h:t) (x,y-1) /= Wall && (o /= R || mazeShowX (h:t) (x-1,y-1) /= Wall) = Move (getPlayerID g) L
                            | mazeShowX (h:t) (x+1,y) /= Wall = Move (getPlayerID g) D
                            | otherwise = Move (getPlayerID g) L

-- *Funções auxiliares para a movimentação para fora da Casa dos /Ghosts/
-- |Função que deteta se o /Ghost/ está dentro da Casa dos /Ghosts/.
insideHouse :: Coords -- ^Coordenada inicial da Casa dos /Ghosts/
                -> Coords -- ^Coordenada final da Casa dos /Ghosts/
                -> Coords -- ^Coordenada do /Ghost/
                -> Bool -- ^Verificação se está dentro ou não
insideHouse (xI,yI) (xF,yF) (gX,gY) | gX == xI && gY > yI && gY < yF = True
                                    | gX == xI+1 && gY > yI && gY < yF = True
                                    | otherwise = False

-- |Função que devolve a posição final da Casa dos /Ghosts/.
finalCoordCF :: Maze -- ^Labirinto
                -> Coords -- ^Coordenada inicial da Casa dos /Ghosts/
                -> Coords -- ^Coordenada final da Casa dos /Ghosts/
finalCoordCF (h:t) (x,y)    | (length h) `mod` 2 == 0 = (x+2,y+7)
                            | otherwise = (x+2,y+8)

-- |Função que determina o movimento ideal para o /Ghost/ sair da Casa dos /Ghosts/.
mExit :: Maze -- ^Labirinto
        -> Player -- ^/Ghost/
        -> Player -- ^/Pacman/
        -> Play -- ^Jogada do /Ghost/
mExit (h:t) g p | pi == 0 && (y == yE || y == yE+1) = Move (getPlayerID g) U
                | pi == 1 && (y == yE || y == yE+1 || y == yE+2) = Move (getPlayerID g) U
                | y < yE && x == xE+1 = Move (getPlayerID g) R
                | y > yE && x == xE+1 = Move (getPlayerID g) L
                | otherwise = Move (getPlayerID g) U
        where   (a,b) = getPlayerCoords g
                (x,y) = (a+1,b+1)
                (xE,yE) = houseExit (h:t) (procurarAlturaCF (length (h:t))+2,procurarLarguraCF (length h)+2)
                pi = mazePIc (h:t)