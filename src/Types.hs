module Types where

import Data.List

-- Tarefa 1
type Maze = [Corridor]
type Corridor = [Piece]

data Piece = Food FoodType | Wall | Empty | PacPlayer Player deriving (Eq)
data FoodType = Big | Little deriving (Eq)

-- Tarefa 2
data Play = Move Int Orientation deriving (Eq, Show)
data Orientation = L | R | U | D | Null deriving (Eq, Show)
data Player = Pacman PacState | Ghost GhoState deriving (Eq)

--                 (ID, (x,y), velocity, orientation, points, lives)
type PlayerState = (Int, Coords, Double, Orientation, Int, Int)
type Coords = (Int, Int)

data State = State {
    maze :: Maze,
    playersState :: [Player],
    level :: Int
} deriving (Eq)

data PacState = PacState {
    pacState :: PlayerState,
    timeMega :: Double,
    openClosed :: Mouth,
    pacmanMode :: PacMode
} deriving (Eq)
data Mouth = Open | Closed deriving (Eq, Show)
data PacMode = Dying | Mega | Normal deriving (Eq, Show)

data GhoState = GhoState {
    ghostState :: PlayerState,
    ghostMode :: GhostMode
} deriving (Eq)
data GhostMode = Dead | Alive deriving (Eq, Show)

data Color = Blue | Green | Purple | Red | Yellow | None deriving (Eq)

-- Tarefa 3

type Instructions = [Instruction]

data Instruction = Instruct [(Int, Piece)] | Repeat Int deriving (Eq, Show)

-- Instance Show Functions

instance Show State where
    show (State m ps p) = printMaze mz ++ "Level: " ++ show p ++ "\nPlayers: \n" ++ (foldr (++) "\n" (map (\y -> printPlayerStats y) ps))
        where mz = placePlayersOnMap ps m

instance Show PacState where
    show (PacState s o m Dying) = "X"
    show (PacState (a,b,c,R,i,l) _ Open m) = "{"
    show (PacState (a,b,c,R,i,l) _ Closed m) = "<"
    show (PacState (a,b,c,L,i,l) _ Open m) = "}"
    show (PacState (a,b,c,L,i,l) _ Closed m) = ">"
    show (PacState (a,b,c,U,i,l) _ Open m) = "V"
    show (PacState (a,b,c,U,i,l) _ Closed m) = "v"
    show (PacState (a,b,c,D,i,l) _ Open m) = "^"
    show (PacState (a,b,c,D,i,l) _ Closed m) = "|"
    show (PacState (a,b,c,Null,i,l) _ Open m) = "{"
    show (PacState (a,b,c,Null,i,l) _ Closed m) = "<"

instance Show Player where
    show (Pacman x) = show x
    show (Ghost x) = show x

instance Show GhoState where
    show (GhoState x Dead) = "?"
    show (GhoState x Alive) = "M"

instance Show FoodType where
    show (Big) = "o"
    show (Little) = "."

instance Show Piece where
    show (Wall) = coloredString "#" None
    show (Empty) = coloredString " " None
    show (Food z) = coloredString (show z) Green
    show (PacPlayer (Pacman (PacState (i, c, x, y, z, l) o m Normal))) = coloredString (show (PacState (i, c, x, y, z, l) o m Normal)) Yellow
    show (PacPlayer (Pacman (PacState (i, c, x, y, z, l) o m Mega))) = coloredString (show (PacState (i, c, x, y, z, l) o m Mega)) Blue
    show (PacPlayer (Pacman (PacState (i, c, x, y, z, l) o m Dying))) = coloredString (show (PacState (i, c, x, y, z, l) o m Dying)) Red
    show (PacPlayer (Ghost z)) = coloredString (show z) Purple

-- OTHER FUNCTIONS

coloredString :: String -> Color -> String
coloredString x y   
                    | True = x
                    | y == Blue = "\x1b[36m" ++ x ++ "\x1b[0m"
                    | y == Red = "\x1b[31m" ++ x ++ "\x1b[0m"
                    | y == Green = "\x1b[32m" ++ x ++ "\x1b[0m"
                    | y == Purple = "\x1b[35m" ++ x ++ "\x1b[0m"
                    | y == Yellow = "\x1b[33m" ++ x ++ "\x1b[0m"
                    | otherwise = "\x1b[0m" ++ x

placePlayersOnMap :: [Player] -> Maze -> Maze
placePlayersOnMap [] x = x
placePlayersOnMap (h:t) m = placePlayersOnMap t (replaceElemInMaze (getPlayerCoords h) (PacPlayer h) m)

printMaze :: Maze -> String
printMaze [] = ""
printMaze (h:t) = foldr (++) "" (map (\y -> show y) h) ++ "\n" ++ printMaze (t)

printPlayerStats :: Player -> String
printPlayerStats p =
    let (a, b, c, d, e, l) = getPlayerState p
    in "ID:" ++ show a ++ " Points:" ++ show e ++ " Lives:" ++ show l ++ "\n"

-- Função apenas para teste
--RETIRAR ANTES DA ENTREGA
printPlayerTM :: Player -> String
printPlayerTM p =
    let tm = getPacmanTime p
        md = getPacmanMode p
    in "Time:" ++ show tm ++ " Mode:" ++ show md ++ "\n"

getPlayerID :: Player -> Int
getPlayerID (Pacman (PacState (x, y, z, t, h, l) q c d)) = x
getPlayerID (Ghost (GhoState (x, y, z, t, h, l) q)) = x

getPlayerPoints :: Player -> Int
getPlayerPoints (Pacman (PacState (x, y, z, t, h, l) q c d)) = h
getPlayerPoints (Ghost (GhoState (x, y, z, t, h, l) q)) = h

setPlayerCoords :: Player -> Coords -> Player
setPlayerCoords (Pacman (PacState (x, y, z, t, h, l) q c d)) (a, b) = Pacman (PacState (x, (a, b), z, t, h, l) q c d)
setPlayerCoords (Ghost (GhoState (x, y, z, t, h, l) q)) (a, b) = Ghost (GhoState (x, (a, b), z, t, h, l) q)

getPieceOrientation :: Piece -> Orientation
getPieceOrientation (PacPlayer p) = getPlayerOrientation p
getPieceOrientation _ = Null

setPlayerOrientation :: Player -> Orientation -> Player
setPlayerOrientation (Pacman (PacState (x, y, z, t, h, l) q c d)) nO = Pacman (PacState (x, y, z, nO, h, l) q c d)
setPlayerOrientation (Ghost (GhoState (x, y, z, t, h, l) q)) nO = Ghost (GhoState (x, y, z, nO, h, l) q)

getPacmanMode :: Player -> PacMode
getPacmanMode (Pacman (PacState a b c d)) = d

-- Função apenas para teste
--RETIRAR ANTES DA ENTREGA
getPacmanTime :: Player -> Double
getPacmanTime (Pacman (PacState a b c d)) = b

getPlayerState :: Player -> PlayerState
getPlayerState (Pacman (PacState a b c d)) = a
getPlayerState (Ghost (GhoState a b)) = a

getPlayerOrientation :: Player -> Orientation
getPlayerOrientation (Pacman (PacState (x, y, z, t, h, l) q c d)) = t
getPlayerOrientation (Ghost (GhoState (x, y, z, t, h, l) q)) = t

getPlayerCoords :: Player -> Coords
getPlayerCoords (Pacman (PacState (x, y, z, t, h, l) q c d)) = y
getPlayerCoords (Ghost (GhoState (x, y, z, t, h, l) q)) = y

replaceElemInMaze :: Coords -> Piece -> Maze -> Maze
replaceElemInMaze (a, b) _ [] = []
replaceElemInMaze (a, b) p (h:t)    | a == 0 = (replaceNElem b p h):t
                                    | otherwise = h:(replaceElemInMaze (a-1, b) p t)

replaceNElem :: Int -> a -> [a] -> [a]
replaceNElem i _ [] = []
replaceNElem i el (h:t) | i == 0 = el:t
                        | otherwise = h:(replaceNElem (i-1) el t)