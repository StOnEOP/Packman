{-|
Module      : Main
                
Este ficheiro contém __a função principal do jogo__ e todos os mecanismos usados.
-}
module Main where

import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import Types
import FileUtils
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Tarefa5
import Tarefa6


data Manager = Manager 
    {   
        state   :: State
    ,    pid    :: Int
    ,    step   :: Int
    ,    before :: Integer
    ,    delta  :: Integer
    ,    delay  :: Integer
    }

-- |Função que lê a data /Manager/.
loadManager :: Manager -- ^/Manager/
loadManager = ( Manager (loadMaze "maps/1.txt") 0 0 0 0 defaultDelayTime)

-- |Função que lê a data /Manager/ mas que recebe um labirinto.
loadManager2 :: Maze -- ^Labirinto
             -> Manager -- ^/Manager/
loadManager2 m = (Manager (State m (generatePlayerList m) 1) 0 0 0 0 defaultDelayTime)

-- |Função que gera uma lista de três jogadores.
generatePlayerList :: Maze -- ^Labirinto
                -> [Player] -- ^Lista de jogadores
generatePlayerList m = [(Ghost (GhoState (1, (middleCoordMaze m), 0, Null, 0, 1) Alive)),
                        (Ghost (GhoState (2, (middleCoordMaze m), 0, Null, 0, 1) Dead)),
                        (Pacman (PacState (0, (x+2,y), 1, R, 0, 1) 0 Open Normal))]
                        where (x,y) = (middleCoordMaze m)

-- |Função que controla o input do jogador.
updateControlledPlayer :: Key -- ^Tecla
                    -> Manager -- ^/Manager/ atual
                    -> Manager -- ^/Manager/ novo
updateControlledPlayer k m  | k == KeyUpArrow = m {state = State a b1 c}
                            | k == KeyDownArrow = m {state = State a b2 c}
                            | k == KeyRightArrow = m {state = State a b3 c}
                            | otherwise = m {state = State a b4 c}
                        where   a = maze(state m)
                                b1 = changePS (playersState(state m)) (setPlayerOrientation (findPlayer (state m) (pid m)) U)
                                b2 = changePS (playersState(state m)) (setPlayerOrientation (findPlayer (state m) (pid m)) D)
                                b3 = changePS (playersState(state m)) (setPlayerOrientation (findPlayer (state m) (pid m)) R)
                                b4 = changePS (playersState(state m)) (setPlayerOrientation (findPlayer (state m) (pid m)) L)
                                c = level(state m)

-- |Função que atualiza a /Window/ do /nCurses/.
updateScreen :: Window -- ^/Window/ atual
            -> ColorID -- ^ID da cor
            -> Manager -- ^/Manager/ atual
            -> Curses () -- ^/Window/ nova
updateScreen w a man = do
                    updateWindow w $ do
                        clear
                        setColor a
                        moveCursor 0 0 
                        drawString $ show (state man)
                        drawString $ "Step: " ++ show (step man)
                    render
     
-- |Função que retorna o tempo atual.
currentTime :: IO Integer -- ^Tempo atual
currentTime = fmap ( round . (* 1000) ) getPOSIXTime

-- |Função que atualiza o tempo.
updateTime :: Integer -- ^Tempo atual
            -> Manager -- ^/Manager/ atual
            -> Manager -- ^/Manager/ novo
updateTime now (Manager state p step b delta delay) = Manager state p step now (delta+(now-b)) delay

-- |Função que dá reset ao tempo.
resetTimer :: Integer -- ^Tempo atual
            -> Manager -- ^/Manager/ atual
            -> Manager -- ^/Manager/ novo
resetTimer now man = man {delta = 0, before = now}

-- |Função que invoca o jogo principal e que aumenta os steps.
nextFrame :: Integer -- ^Tempo atual
        -> Manager -- ^/Manager/ atual
        -> Int -- ^Modo de jogo
        -> Int -- ^Pausa
        -> Manager -- ^/Manager/ novo
nextFrame now man modo pause = resetTimer now man {state = passTime (step man) (state man) modo pause, step = (step man)+1}-- (tempo entre jogadas excede o limite e temos de fazer uma jogada automatica)

-- |Função que aumenta o nível do jogo.
nextLevel :: Manager -- ^/Manager/ atual
        -> Manager -- ^/Manager/ novo
nextLevel man = nextLevelAUX (level(state man)) (Manager (loadMaze "maps/1.txt") 0 0 0 0 defaultDelayTime)

-- |Função que invoca a próxima função necessária para completar a passagem de nível.
nextLevelAUX :: Int -- ^Nível do jogo
            -> Manager -- ^/Manager/ atual
            -> Manager -- ^/Manager/ novo
nextLevelAUX nivel (Manager state p step b delta delay) = (Manager (nextLevelAUX2 nivel state) p step b delta delay)

-- |Função que altera o nível no estado do jogo.
nextLevelAUX2 :: Int -- ^Nível do jogo
            -> State -- ^Estado atual
            -> State -- ^Estado novo
nextLevelAUX2 nivel (State a b level) = State a b (level+nivel) 

-- |Função que invoca as funções do jogo.
loop :: Int -- ^Modo de jogo
    -> Int -- ^Pausa
    -> Window -- ^/Window/ atual
    -> Manager -- ^/Manager/ atual
    -> Curses () -- ^/Window/ nova
loop modo pause w man@(Manager s pid step bf delt del) = do 
  color_schema <- newColorID ColorBlue ColorWhite  10
  now <- liftIO  currentTime
  updateScreen w  color_schema man
  if (isThereFood(maze(state man)) == False) then loop modo pause w $ nextLevel man
    else if (delt > del)
      then loop modo pause w $ nextFrame now man modo pause
      else do
            ev <- getEvent w $ Just 0
            case ev of
                  Nothing -> loop modo pause w (updateTime now man)
                  Just (EventSpecialKey arrow ) -> loop modo pause w $ updateControlledPlayer arrow (updateTime now man)
                  Just ev' ->
                    if (ev' == EventCharacter 'q')
                      then return ()
                    else  if (ev' == EventCharacter 'p')
                            then  if (pause == 0)
                                      then loop modo 1 w (updateTime now man)
                                  else loop modo 0 w (updateTime now man)
                          else loop modo 0 w (updateTime now man)

-- |Função principal.
main :: IO ()
main = do
    putStrLn "\n--- PAC-MAN ---"
    putStrLn "1- Jogo manual\n2- Jogo automático\n3- Intruções\n4- Sair\n"
    line <- getLine
    case line of
        "1" ->  runCurses $ do
                    setEcho False
                    setCursorMode CursorInvisible
                    w <- defaultWindow
                    loop 1 0 w loadManager
        "2" ->  runCurses $ do
                    setEcho False
                    setCursorMode CursorInvisible
                    w <- defaultWindow
                    loop 2 0 w loadManager
        "3" ->  instructions
        "4" ->  exit

-- |Função principal para a utilização de um labirinto criada pela Tarefa1.
mainM :: Maze -- ^Labirinto
     -> IO ()
mainM m = do
  putStrLn "\n--- PAC-MAN ---"
  putStrLn "Jogo manual com labirinto gerado\n"
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop 1 0 w (loadManager2 m)

-- |Função para verificar as definições.
instructions :: IO ()
instructions = do
    putStrLn ("\n\n\n\nInstruções")
    putStrLn ("  - Clica nas setas do teclado para conseguires jogar")
    putStrLn ("  - Clica 'Q' durante o jogo para o terminares")
    putStrLn ("  - Clica 'P' durante o jogo para o pausares\n")
    putStrLn ("  Voltar\n\n\n\n")
    line <- getLine
    main
    
-- |Função para terminar o jogo.
exit :: IO ()
exit = do
    putStrLn ("\nJogo terminado.")