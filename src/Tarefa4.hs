{- |

Module      : Tarefa 4
Copyright   :   André Silva
                João Nunes
    
                
= Introdução

Nesta tarefa tivemos de fazer jogar os jogadores (/Pacman/ e /Ghosts/) consoante a passagem de tempo (Steps).


= Objetivos

O objetivo principal era fazer jogar os jogadores, jogando primeiro o /Pacman/ e de seguida os /Ghosts/.
Para isto usamos a Tarefa2 já anteriormente desenvolvida, usando primeiramente a função play para o /Pacman/
e de seguida para todos os /Ghosts/.
O movimento do /Pacman/ pode funcionar de duas formas, através dos inputs do jogador ou através de um bot
desenvolvido na Tarefa6. Criamos então uma função movePacmanBot que ao invés de utilizar a função play da
Tarefa2 com a orientação do /Pacman/ dada pelo utilizador, vai a Tarefa6 buscar as jogadas que o bot vai
realizar e então chamar a função play da Tarefa2 para essas jogadas.
Um outro objetivo era a introdução de diferentes velocidades possíveis para os /Ghosts/, de modo a podermos
ter um fator de dificuldade, aumentando a velocidade dos mesmos a cada nível.


= Discussão e Conclusão

Acreditamos que a tarefa cumpre com o seu principal objetivo, fazer jogar os jogadores e a 
opção de um bot para mover o /Pacman/, falhando apenas no facto referido acima das velocidades, 
que após varias tentativas, não foi possivel implementar e, portanto, todos os jogadores se mexem à
mesma velocidade.
-}

module Tarefa4 where 

import Types
import Tarefa2
import Tarefa5
import Tarefa6


defaultDelayTime = 250 -- 250 ms

-- *Funções principais para a realização de todas as jogadas de todas as maneiras
-- |Função que consoante a passagem de tempo, faz jogar todos os jogadores.
passTime :: Int -- ^Steps
        -> State -- ^Estado atual
        -> Int -- ^Modo de jogo
        -> Int -- ^Pausa
        -> State -- ^Estado novo
passTime x s modo pause | modo == 1 && getPacmanMode(getPacman(playersState s)) /= Dying && pause == 0 = moveGhosts (ghostPlay s) (movePacman s)
                        | modo == 2 && getPacmanMode(getPacman(playersState s)) /= Dying && pause == 0 = moveGhosts (ghostPlay s) (movePacmanBot (bot id s) s)
                        | otherwise = s
                    where   p = getPacman (playersState s)
                            id = getPlayerID p

-- |Função que realiza uma jogada do /Pacman/.
movePacman :: State -- ^Estado atual
            -> State -- ^Estado novo
movePacman s = play (Move (getPlayerID (getPacman (playersState s))) (getPlayerOrientation(getPacman (playersState s)))) s

-- |Função que permite realizar todas as jogadas pretendidas do bot dos /Ghost/.
moveGhosts :: [Play] -- ^Lista de todas as jogadas dos /Ghost/
            -> State -- ^Estado atual
            -> State -- ^Estado novo
moveGhosts [] s = s
moveGhosts (h:t) s = moveGhosts t (play (h) s)

-- |Função que permite realizar a jogada pretendida do bot do /Pacman/.
movePacmanBot :: Maybe Play -- ^Jogada do /Pacman/
                -> State -- ^Estado atual
                -> State -- ^Estado novo
movePacmanBot Nothing s = s
movePacmanBot (Just p) s = play p s