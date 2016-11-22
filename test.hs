module Main where

import           Control.Concurrent
import           Data.Char
import           Data.List
import           Foreign.C.Types
import           System.Console.ANSI
getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

main :: IO ()
main = do
    threadDelay 100000000
    level <- loadLevel "level1.txt"
    drawLevel level
    gameLoop level

type Coord = (Integer, Integer)

data Level = Level {walls    :: [Coord]
                   ,crates   :: [Coord]
                   ,player   ::  Coord
                   ,storages :: [Coord]
                   } deriving (Show)

data Input = IUp | IDown | ILeft | IRight deriving (Eq, Show)

add :: Coord -> Input -> Coord
add (x, y) input
    | input == IUp    = (x, y - 1)
    | input == IDown  = (x, y + 1)
    | input == ILeft  = (x - 1, y)
    | input == IRight = (x + 1, y)
    | otherwise       = (x, y)

getInput :: IO Input
getInput = do
    nChar <- getHiddenChar
    case nChar of
        'w' -> return IUp
        's' -> return IDown
        'a' -> return ILeft
        'd' -> return IRight
        _   -> getInput

moveCharacter :: Level -> Input -> Level
moveCharacter lvl input
    | isAtCoord newPos lvl walls  = lvl
    | isAtCoord newPos lvl crates =
        if isAtCoord newPos' lvl walls || isAtCoord newPos' lvl crates
            then lvl
            else lvl{player = newPos
                    ,crates = newPos':delete newPos (crates lvl)}
    | otherwise = lvl{player = newPos}
        where
            newPos  = add (player lvl) input
            newPos' = add newPos input


emptyLevel :: Level
emptyLevel = Level {walls    = []
                   ,crates   = []
                   ,player   = (0,0)
                   ,storages = []
                   }

drawLevel :: Level -> IO ()
drawLevel level = do
    clearScreen
    putStr toString
    where
        toString :: String
        toString = unlines (map fillLines toLines)
        toLines = groupBy byY (sortBy comp allElements)
        comp ((ax,ay),_) ((bx,by),_) | ay < by = LT
                                     | by < ay = GT
                                     | ay == by = if ax < bx then LT
                                                    else if bx < ax then GT
                                                        else EQ
                                     | otherwise = EQ
        fillLines = fillLines' 0
        fillLines' :: Integer -> [(Coord, Char)] -> String
        fillLines' i (x:xs) = if i == fst (fst x) then snd x : fillLines' (i + 1) xs else ' ' : fillLines' (i + 1) (x:xs)
        fillLines' _ [] = []
        byY ((_,ya),_) ((_,yb), _) = ya == yb
        allElements :: [(Coord, Char)]
        allElements = map (\x -> (x,'#')) (walls level) ++ map (\x -> (x,'@')) (crates level) ++ map (\x -> (x,'x')) filteredStorages' ++ [(player level, player')]
        filteredStorages :: [Coord]
        filteredStorages = delete (player level) (storages level)
        filteredStorages' :: [Coord]
        filteredStorages' = filter isNElem filteredStorages
        isNElem :: Coord -> Bool
        isNElem x = notElem x (crates level)
        player' = if elem (player level) (storages level) then 'R' else 'P'

isAtCoord :: Coord -> Level -> (Level -> [Coord]) -> Bool
isAtCoord coord lvl f = elem coord (f lvl)

loadLevel :: String -> IO Level
loadLevel path = do
    level <- readFile path
    return (foldl addToWorld emptyLevel (elems level))
        where
            elems lvl = concat $ zipWith zip coords (lns lvl)
            lns = lines
            coords = [[(x,y) | x <- [0..]] | y <- [0..]]
            addToWorld lvl (c, ch)
                | ch == '#' = lvl {walls       = c:walls lvl   }
                | ch == 'P' = lvl {player      = c             }
                | ch == 'x' = lvl {storages    = c:storages lvl}
                | ch == '@' = lvl {crates      = c:crates lvl  }
                | otherwise = lvl

isDone :: Level -> Bool
isDone level = sort (crates level) == sort (storages level)

gameLoop :: Level -> IO ()
gameLoop level = do
    input <- getInput
    drawLevel (newLevel' input)
    if isDone (newLevel' input)
    then putStrLn "You Won!"
    else gameLoop (newLevel' input)
        where
            newLevel' = moveCharacter level
