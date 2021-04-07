{-|
Module      : Tarefa 2
Copyright   :   André Silva
                João Nunes
                
O objetivo deste módulo é a __realização de uma jogada__, consoante a orientação dessa mesma e principalmente o estado do jogo.
-}
module Tarefa2 where
import Types
import Tarefa1


-- *Realização da jogada
-- |Função principal que realiza a jogada pretendida.
play :: Play -- ^Jogada pretendida
        -> State -- ^Estado inicial
        -> State -- ^Estado final
play (Move id o) s =
    let p = findPlayer s id
        pAUX = findPacmanPlayer (playersState s)
        nP = mainMove (playersState s) (maze s) p o
        slP = State (maze s) (changePS (playersState s) nP) (level s)
        pP = findPacmanPlayer (playersState slP)
        gP = mazeEatPacman pAUX pP p nP
        tP = changeMegaTM gP
        newS = State (maze slP) (changePS (playersState slP) tP) (level slP)
        slPF = mainGhost tP (getPlayerPoints pAUX) (getPlayerPoints tP) newS (getPlayerCoords tP)
        changeState = State (foodToEmpty (maze slPF) nP (getPlayerCoords p)) (playersState slPF) (level slPF)
    in changeState

-- *Funções auxiliares  
-- |Função que indica qual é o estado do jogador que queremos mover.
findPlayerState :: State -- ^Estado
                    -> Int -- ^ID do jogador que queremos
                    -> PlayerState -- ^Estado do jogador certo
findPlayerState s id = getPlayerState (findPlayer s id)

-- |Função que retorna o jogador pretendido.
findPlayer :: State -- ^Estado
                -> Int -- ^ID do jogador que queremos
                -> Player -- ^Jogador pretendido
findPlayer s id = findPlayerAUX id lPlayers
    where lPlayers = playersState s

-- |Função auxiliar que encontra o jogador que queremos mover.
findPlayerAUX :: Int -- ^ID do jogador que queremos
                -> [Player] -- ^Lista de todos os jogadores
                -> Player -- ^Jogador pretendido
findPlayerAUX id (h:t)  | (getPlayerID h) == id = h
                        | otherwise = findPlayerAUX id t

-- |Função que encontra o /Pacman/.
findPacmanPlayer :: [Player] -- ^Lista de todos os jogadores
                -> Player -- ^Jogador /Pacman/
findPacmanPlayer ((Ghost h):t) = findPacmanPlayer t
findPacmanPlayer ((Pacman h):t) = Pacman h

-- |Função que altera a lista de jogadores de modo a inserir o jogador que foi alterado.
changePS :: [Player] -- ^Lista de todos os jogadores
            -> Player -- ^Jogador novo
            -> [Player] -- ^Lista de todos os jogadores com o novo jogador já inserido
changePS [] _ = []
changePS (h:t) p    | (getPlayerID h) == (getPlayerID p) = p:t
                    | otherwise = h:changePS t p

-- |Função que verifica se o comprimento do labirinto é par ou ímpar, devolve um inteiro.
--
--  * 0, par
--  * 1, ímpar
mazePIc :: Maze -- ^Labirinto
        -> Int -- ^Variável auxiliar
mazePIc (h:t)   | (length h) `mod` 2 == 0 = 0
                | otherwise = 1

-- |Função que verifica se a altura do labirinto é par ou ímpar, devolve um inteiro.
--
--  * 0, par
--  * 1, ímpar
mazePIa :: Maze -- ^Labirinto
        -> Int -- ^Variável auxiliar
mazePIa (h:t)   | (length (h:t)) `mod` 2 == 0 = 0
                | otherwise = 1

-- |Função que descobre a coordenada da saída da Casa dos /Ghosts/.
houseExit :: Maze -- ^Labirinto
            -> Coords -- ^Coordenada inicial da Casa dos /Ghosts/
            -> Coords -- ^Coordenada da saída
houseExit (h:t) (x,y) = (x,y+3)

-- *Mudança de estados dos /Ghosts/
-- |Função que chama as respetivas funções dependendo das seguintes situações:
--
--  * diferença entre a pontuação anterior e a nova == 5 que significa que o jogador comeu um /Food Big/
--  * diferença entre a pontuação anterior e a nova == 11 || == 10 que significa que o jogador comeu um /Ghost/ e um /Food Little/ ou apenas um /Ghost/
--  * diferença entre a pontuação anterior e a nova >= 15 que significa todos os outros casos possíveis, que vão ser analisados na respetiva função
mainGhost :: Player -- ^Jogador novo
            -> Int -- ^Pontuação anterior
            -> Int -- ^Pontuação nova
            -> State -- ^Estado novo
            -> Coords -- ^Coordenadas do jogador
            -> State -- ^Estado com a alteração dos estados dos /Ghosts/
mainGhost p pBefore pAfter (State m pS l) (x,y) | pAfter - pBefore == 5 = (State m (ghostTo pS 1) l)
                                                | pAfter - pBefore == 11 || pAfter - pBefore == 10 = (State m (changeGhostState p m pS (x,y) 1) l)
                                                | pAfter - pBefore >= 15 = (State m (changeGhostState p m pS (x,y) 2) l)
                                                | otherwise = (State m (checkPacmanTM p pS) l)

-- |Função que chama as respetivas funções dependendo da situação do /Ghost/ e da variável recebida.
changeGhostState :: Player -- ^Jogador
                    -> Maze -- ^Labirinto
                    -> [Player] -- ^Lista de todos os jogadores
                    -> Coords -- ^Coordenada do jogador
                    -> Int -- ^Varíavel recebida
                    -> [Player] -- ^Lista de jogadores com os /Ghosts/ alterados
changeGhostState p m [] (x,y) n             = []               
changeGhostState p m ((Pacman h):t) (x,y) n = (Pacman h):(changeGhostState p m t (x,y) n)
changeGhostState p m ((Ghost h):t) (x,y) n  | n == 1 && getPlayerCoords (Ghost h) == (x,y) = (changeGhostStateAUX m (Ghost h) 1): changeGhostState p m t (x,y) n
                                            | n == 2 = (changeGhostStateALL p m ((Ghost h):t) (x,y))
                                            | otherwise = (Ghost h):(changeGhostState p m t (x,y) n)

-- |Função que altera o estado de todos os /Ghosts/ consoante, as várias condições propostas:
--
--  * se /Ghost/ está na posição certa e se jogador está em modo MEGA ou NORMAL
--  * se jogador está em modo MEGA apenas
changeGhostStateALL :: Player -- ^Ĵogador
                    -> Maze -- ^Labirinto
                    -> [Player] -- ^Lista de todos os jogadores
                    -> Coords -- ^Coordenada do jogador
                    -> [Player] -- ^Lista de jogadores com os /Ghosts/ alterados
changeGhostStateALL p m [] (x,y) = []                
changeGhostStateALL p m ((Pacman h):t) (x,y) = (Pacman h):(changeGhostStateALL p m t (x,y))
changeGhostStateALL p m ((Ghost h):t) (x,y) | getPlayerCoords (Ghost h) == (x,y) && (getPacmanMode p == Normal) = (changeGhostStateAUX m (Ghost h) 1):(changeGhostStateALL p m t (x,y))
                                            | getPlayerCoords (Ghost h) == (x,y) && (getPacmanMode p == Mega) = (changeGhostStateAUX m (Ghost h) 2):(changeGhostStateALL p m t (x,y))
                                            | (getPacmanMode p == Mega) = (changeGhostStateAUX m (Ghost h) 3):(changeGhostStateALL p m t (x,y))
                                            | otherwise = Ghost h:changeGhostStateALL p m t (x,y)

-- |Função que altera o estado de um único /Ghost/, consoante a variável recebida.
changeGhostStateAUX :: Maze -- ^Labirinto
                    -> Player -- ^/Ghost/ pretendido
                    -> Int -- ^Variável recebida
                    -> Player -- ^/Ghost/ novo
changeGhostStateAUX m (Ghost (GhoState (id, c, vel, o, pnt, v) md)) 1 = Ghost (GhoState (id, (middleCoordMaze m), vel, o, pnt, v) Alive)
changeGhostStateAUX m (Ghost (GhoState (id, c, vel, o, pnt, v) md)) 2 = Ghost (GhoState (id, (middleCoordMaze m), vel*2, o, pnt, v) Alive)
changeGhostStateAUX m (Ghost (GhoState (id, c, vel, o, pnt, v) md)) 3 = Ghost (GhoState (id, c, vel/2, o, pnt, v) Dead)

-- |Função que encontra as coordenadas, na casa dos fantasmas, para a qual o /Ghost/ se move quando morre.
middleCoordMaze :: Maze -- ^Labirinto
                -> Coords -- ^Coordenadas do meio do labirinto
middleCoordMaze (h:t)   | ((heightMaze (h:t)) `mod` 2) /= 0 = (((heightMaze (h:t)) `div` 2),(lengthMaze h) `div` 2)
                        | otherwise = (((heightMaze (h:t)) `div` 2)-1,(lengthMaze h) `div` 2)

-- |Função que determina a altura do labirinto.
heightMaze :: Maze -- ^Labirinto
            -> Int -- ^Altura do labirinto
heightMaze [] = 0
heightMaze (h:t) = 1 + heightMaze t

-- |Função que determina a largura do labirinto.
lengthMaze :: Corridor -- ^Corredor
            -> Int -- ^Largura do labirinto
lengthMaze [] = 0
lengthMaze (h:t) = 1 + lengthMaze t

-- |Função que verifica se o modo do pacman se encontra em Mega. Chama, depois, a função pretendida dependendo do resultado.
checkPacmanTM :: Player -- ^Jogador
                -> [Player] -- ^Lista de todos os jogadores
                -> [Player] -- ^Lista de todos os jogadores com, possivelmente, os estados alterados
checkPacmanTM p pS  | (getPacmanMode p) == Normal = (ghostTo pS 2)
                    | otherwise = pS

-- |Função que muda todos os /Ghosts/ para o estado desejado.
ghostTo :: [Player] -- ^Lista de todos os jogadores
            -> Int -- ^Variável que decide o estado desejado
            -> [Player] -- ^Lista de todos os jogadores já com os estados alterados
ghostTo [] _ = []
ghostTo ((Pacman h):t) x = (Pacman h):(ghostTo t x)
ghostTo ((Ghost h):t) x = (ghostToAUX (Ghost h) x):(ghostTo t x)

-- |Função que muda um /Ghost/ para o estado desejado dependendo da variável escolhida:
-- 
--  * 1: ghostToDead
--  * 2: ghostToAlive
ghostToAUX :: Player -- ^/Ghost/
            -> Int -- ^Variável que decide o estado desejado
            -> Player -- ^/Ghost/ novo
ghostToAUX (Ghost (GhoState (id, c, vel, o, pnt, v) md)) 1 = Ghost (GhoState (id, c, vel/2, o, pnt, v) Dead)
ghostToAUX (Ghost (GhoState (id, c, vel, o, pnt, v) md)) 2 = Ghost (GhoState (id, c, vel*2, o, pnt, v) Alive)

-- *Movimentação de /Pacman/ ou /Ghost/
-- |Função que testa se o jogador está em modo /Dying/ e se a orientação da jogada pretendida é igual à sua orientação. Chama, depois, as respetivas funções.
mainMove :: [Player] -- ^Lista de todos os jogadores
                -> Maze -- ^Labirinto
                -> Player -- ^Jogador
                -> Orientation -- ^Orientação da jogada
                -> Player -- ^Jogador com o seu estado atualizado
mainMove lP m (Ghost p) nO  | (getPlayerOrientation (Ghost p)) == nO = nextCoord lP (Ghost p) m
                            | otherwise = setPlayerOrientation (Ghost p) nO
mainMove lP m (Pacman p) nO | (getPacmanMode (Pacman p)) == Dying = Pacman p
                            | (getPlayerOrientation (Pacman p)) == nO = nextCoord lP (Pacman p) m
                            | otherwise = changeMouth nP
                        where nP = setPlayerOrientation (Pacman p) nO

-- |Função que determina a coordenada para a qual o jogador se quer mover. Chama, também, a função certa quando essa mesma coordenada.
-- Passamos as coordenadas para coordenadas de labirinto, ou seja, incrementamos 1 em cada uma das coordenadas do tuplo.
nextCoord :: [Player] -- ^Lista de todos os jogadores
            -> Player -- ^Jogador
            -> Maze -- ^Labirinto
            -> Player -- ^Jogador com o seu estado atualizado
nextCoord lP p m    | o == L = nextMazeCoord lP p m (x+1,y) 
                    | o == R = nextMazeCoord lP p m (x+1,y+2)
                    | o == D = nextMazeCoord lP p m (x+2,y+1)
                    | otherwise = nextMazeCoord lP p m (x,y+1)
                where   o = getPlayerOrientation p
                        (x,y) = getPlayerCoords p

-- |Função que atualiza o estado do jogador para todas as seguintes situações:
--
--  *Se a coordenada pretendida tiver um /Food Little/ ou /Food Big/ ou /Empty/ então vai chamar a função para verificar a situação dos /Ghosts/ nessa mesma coordenada
--  *Se a coordenada pretendida tiver na posição de um túnel, vai chamar a função respetiva
nextMazeCoord :: [Player] -- ^Lista de todos os jogadores
                -> Player -- ^Jogador
                -> Maze -- ^Labirinto
                -> Coords -- ^Coordenadas da jogada
                -> Player -- ^Jogador com o seu estado atualizado
nextMazeCoord lP (Ghost p) (h:t) (nX,nY)    | mazeShowX (h:t) (nX,nY) == Empty && mazeShowT (h:t) (nX,nY) == True = mazeTunnel lP (Ghost p) (h:t) (nX,nY) 0
                                            | mazeShowX (h:t) (nX,nY) /= Wall = setPlayerCoords (Ghost p) (nX-1,nY-1)
                                            | otherwise = Ghost p
nextMazeCoord lP (Pacman p) (h:t) (nX,nY)   | mazeShowX (h:t) (nX,nY) == Food Little = mazeGhost (Pacman p) (nX-1,nY-1) (getGhosts lP) 1
                                            | mazeShowX (h:t) (nX,nY) == Food Big = mazeGhost (Pacman p) (nX-1,nY-1) (getGhosts lP) 5
                                            | mazeShowX (h:t) (nX,nY) == Empty && mazeShowT (h:t) (nX,nY) == True = mazeTunnel lP (Pacman p) (h:t) (nX,nY) 0
                                            | mazeShowX (h:t) (nX,nY) == Empty && mazeShowT (h:t) (nX,nY) == False && pi == 0 && (nX,nY) /= (xE,yE) && (nX,nY) /= (xE,yE+1) = mazeGhost (Pacman p) (nX-1,nY-1) (getGhosts lP) 0
                                            | mazeShowX (h:t) (nX,nY) == Empty && mazeShowT (h:t) (nX,nY) == False && pi == 1 && (nX,nY) /= (xE,yE) && (nX,nY) /= (xE,yE+1) && (nX,nY) /= (xE,yE+2) = mazeGhost (Pacman p) (nX-1,nY-1) (getGhosts lP) 0
                                            | otherwise = changeMouth (Pacman p)
                                        where   (xE,yE) = houseExit (h:t) (procurarAlturaCF (length (h:t))+2,procurarLarguraCF (length h)+2)
                                                pi = mazePIc (h:t)

-- |Função que percorre os corredores do labirinto até chegar ao corredor certo da coordenada pretendida. Chama, depois, a função que acaba a determinação desta peça.
mazeShowX :: Maze -- ^Labirinto
            -> Coords -- ^Coordenada da peça pretendida
            -> Piece -- ^Peça pretendida
mazeShowX [] _ = Empty
mazeShowX (h:t) (1,y) = mazeShowY h y
mazeShowX (h:t) (x,y) = mazeShowX t (x-1,y)

-- |Função que determina a peça presente numa certa coordenada.
mazeShowY :: Corridor -- ^Corredor certo
            -> Int -- ^Largura restante até à coordenada
            -> Piece -- ^Peça pretendida
mazeShowY [] _ = Empty
mazeShowY (h:t) 0 = Empty
mazeShowY (h:t) 1 = h
mazeShowY (h:t) x = if x==((length (h:t))+1) then Empty else mazeShowY t (x-1)

-- |Função que testa se as coordenadas que nos queremos mover se encontram num túnel.
mazeShowT :: Maze -- ^Labirinto
            -> Coords -- ^Coordenadas da jogada
            -> Bool -- ^Booleano pretendido
mazeShowT (h:t) (x,y)   | (y == 0) || (y == ((length h)+1)) = True
                        | otherwise = False

-- |Função que determina a nova coordenada do jogador quando queremos passar o túnel. Chama, depois, a função para verificar a situação dos /Ghosts/ nessa mesma coordenada.
mazeTunnel :: [Player] -- ^Lista de todos os jogadores
            -> Player -- ^Jogador
            -> Maze -- ^Labirinto
            -> Coords -- ^Coordenadas da jogada
            -> Int -- ^Pontuação acrescentar
            -> Player -- ^Jogador com o seu estado atualizado
mazeTunnel lP (Ghost p) (h:t) (nX,nY) n | nY == 0 = setPlayerCoords (Ghost p) (nX-1,(length h)-1)
                                        | otherwise = setPlayerCoords (Ghost p) (nX-1,0)
mazeTunnel lP (Pacman p) (h:t) (nX,nY) n    | nY == 0 = mazeGhost (Pacman p) (nX-1,(length h)-1) (getGhosts lP) n
                                            | otherwise = mazeGhost (Pacman p) (nX-1,0) (getGhosts lP) n

-- |Função que testa se existe um /Ghost/ nas coordenadas que o jogador se quer mover. Se sim, age conforme os modos do jogador e do /Ghost/:
--
-- *Se em modo MEGA e o /Ghost/ em /Dead/ então incrementamos 10 na pontuação
-- *Se em modo MEGA ou NORMAL e o /Ghost/ em /Alive/ então a função que verifica as vidas do jogador é chamada
mazeGhost :: Player -- ^Jogador
            -> Coords -- ^Coordenada pretendida
            -> [Player] -- ^Lista de todos os /Ghost/
            -> Int -- ^Pontuação
            -> Player -- ^Jogador
mazeGhost (Pacman (PacState (id, (x, y), vel, o, pnt, v) tm oC md)) (nX,nY) [] n    | n == 5 = changeMouth (Pacman (PacState (id, (nX,nY), vel, o, pnt+n, v) 10250 oC Mega))
                                                                                    | otherwise = changeMouth (Pacman (PacState (id, (nX,nY), vel, o, pnt+n, v) tm oC md))
mazeGhost (Pacman (PacState (id, (x, y), vel, o, pnt, v) tm oC md)) (nX,nY) (h:t) n | getPlayerCoords h == (nX,nY) && getGhostMode h == Dead && md == Mega = mazeGhost (Pacman (PacState (id, (x,y), vel, o, pnt, v) tm oC md)) (nX,nY) t (n+10)
                                                                                    | getPlayerCoords h == (nX,nY) && getGhostMode h == Alive && md == Mega = lifeChanger (Pacman (PacState (id, (nX,nY), vel, o, pnt+n, v) tm oC md)) 1
                                                                                    | getPlayerCoords h == (nX,nY) && getGhostMode h == Alive && md == Normal = lifeChanger (Pacman (PacState (id, (nX,nY), vel, o, pnt+n, v) tm oC md)) 1
                                                                                    | otherwise = mazeGhost (Pacman (PacState (id, (x,y), vel, o, pnt, v) tm oC md)) (nX,nY) t n

-- |Função que modifica o jogador dependendo da posição do fantasma. Função só chamada quando a jogada é feita por um fantasma.
mazeEatPacman :: Player -- ^Jogador /Pacman/ anterior
                -> Player -- ^Jogador /Pacman/ novo
                -> Player -- ^Jogador /Ghost/ anterior
                -> Player -- ^Jogador /Ghost/ novo
                -> Player -- ^Jogador /Pacman/
mazeEatPacman (Pacman pA) (Pacman pN) (Pacman gA) (Pacman gN) = (Pacman pN)
mazeEatPacman (Pacman pA) (Pacman pN) (Ghost gA) (Ghost gN) | (x,y) == (gX,gY) || (((a,b) == (gX,gY)) && ((x,y) == (aX,bY))) = lifeChanger (Pacman pN) 0
                                                            | otherwise = (Pacman pN)
                                                        where   (x,y) = getPlayerCoords (Pacman pN)
                                                                (a,b) = getPlayerCoords (Pacman pA)
                                                                (gX,gY) = getPlayerCoords (Ghost gN)
                                                                (aX,bY) = getPlayerCoords (Ghost gA)

-- |Função que altera o modo do jogador para /Dying/ caso não tenha mais nenhuma vida.
lifeChanger :: Player -- ^Jogador
            -> Int -- ^Variável auxiliar
            -> Player -- ^Jogador com o estado novo
lifeChanger (Pacman (PacState (id, (x, y), vel, o, pnt, v) tm oC md)) 0 | v-1 == 0 = (Pacman (PacState (id, (x,y), vel, o, pnt, v-1) tm oC Dying))
                                                                        | v-1 < 0 = (Pacman (PacState (id, (x,y), vel, o, pnt, v) tm oC Dying))
                                                                        | otherwise = (Pacman (PacState (id, (x,y), vel, o, pnt, v-1) tm oC md))
lifeChanger (Pacman (PacState (id, (x, y), vel, o, pnt, v) tm oC md)) 1 | v-1 == 0 = changeMouth (Pacman (PacState (id, (x,y), vel, o, pnt, v-1) tm oC Dying))
                                                                        | v-1 < 0 = (Pacman (PacState (id, (x,y), vel, o, pnt, v) tm oC Dying))
                                                                        | otherwise = changeMouth (Pacman (PacState (id, (x,y), vel, o, pnt, v-1) tm oC md))

-- |Função que muda a boca do pacman
changeMouth :: Player -- ^Jogador
            -> Player -- ^Jogador com boca alterada
changeMouth (Ghost s) = (Ghost s)
changeMouth (Pacman (PacState s tm oC md))  | oC == Closed = (Pacman (PacState s tm Open md))
                                            | otherwise = (Pacman (PacState s tm Closed md))

-- |Função que diminui o tempo mega em 250ms
changeMegaTM :: Player -- ^Jogador
            -> Player -- ^Jogador com tempo mega alterado
changeMegaTM (Pacman (PacState s tm oC Normal)) = (Pacman (PacState s 0 oC Normal))
changeMegaTM (Pacman (PacState s tm oC Dying)) = (Pacman (PacState s 0 oC Dying))
changeMegaTM (Pacman (PacState s tm oC Mega))   | (tm-250) <= 0 = (Pacman (PacState s 0 oC Normal))
                                                | otherwise = (Pacman (PacState s (tm-250) oC Mega))

-- |Função que determina a lista de todos os /Ghosts/.
getGhosts :: [Player] -- ^Lista de todos os jogadores
            -> [Player] -- ^Lista de todos os /Ghosts/
getGhosts [] = []
getGhosts ((Pacman state):t) = getGhosts t
getGhosts ((Ghost state):t) = Ghost state: getGhosts t

getPacman :: [Player] -> Player
getPacman [] = Pacman (PacState (100, (0,0), 1, R, 0, 1) 0 Closed Normal)
getPacman ((Pacman state):t) = Pacman state
getPacman (h:t) = getPacman t

-- |Função que determina o modo em que um /Ghost/ está.
getGhostMode :: Player -- ^/Ghost/
            -> GhostMode -- ^Modo do /Ghost/
getGhostMode (Ghost (GhoState a b)) = b

-- *Funções que correspondem ao labirinto   
-- |Função que altera no labirinto a comida que o /Pacman/ comeu por /Empty/.
foodToEmpty :: Maze -- ^Labirinto
            -> Player -- ^Jogador
            -> Coords -- ^Coordenadas pretendidas
            -> Maze -- ^Labirinto com a coordenada alterada
foodToEmpty m (Ghost p) (nX,nY) = m
foodToEmpty m (Pacman p) (nX,nY)    | mazeShowX m (nX+1,nY+1) == Food Little = replaceElemInMaze (nX,nY) Empty m
                                    | mazeShowX m (nX+1,nY+1) == Food Big = replaceElemInMaze (nX,nY) Empty m
                                    | otherwise = m

-- |Função que invoca a função pretendida para percorrer o labirinto, corredor a corredor.
isThereFood :: Maze -- ^Labirinto
            -> Bool -- ^Booleano pretendido
isThereFood [] = False
isThereFood (h:t) = if (isThereFoodAUX h) then True else isThereFood t

-- |Função que percorre o corredor pretendido para verificar se tem alguma comida no labirinto.
--
--  */True/ significa que tem comida no labirinto
--  */False/ significa que não tem nenhuma comida no labirinto
isThereFoodAUX :: Corridor -- ^Corredor do labirinto
                -> Bool -- ^Booleano pretendido
isThereFoodAUX [] = False
isThereFoodAUX (h:t) = if (h /= Empty && h/= Wall) then True else isThereFoodAUX t
