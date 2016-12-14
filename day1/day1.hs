-- Advent of Code day 1
-- Find the shortest path equivalent to a given sequence of movements
-- and print the length of it, using "taxicab geometry"

{-# Language TemplateHaskell #-} -- For Lenses

import Control.Lens

type Move = (Rotation, Int)

data Direction  = North | South | East | West deriving Show
data Position   = Position { _x,_y :: Int, _direction :: Direction } deriving Show
data Rotation   = L | R deriving (Read, Show)

makeLenses '' Position

starting_pos = Position {_x = 0, _y = 0, _direction = North }

-- Step 1: Parse the input into a useful form: Done

parseInput :: String -> [Move]
parseInput rawInput =
    map (parseMove) moveList
    where
        moveList   = words $ filter (/= ',') rawInput


parseMove :: String -> Move
parseMove (dir:num) =
    (read [dir] :: Rotation, read num :: Int)


-- Step 2: Find the resulting coordinates of the given path

followPath :: Position -> [Move] -> Position

-- Recursive base case
followPath pos []           = pos

-- main recursive function
followPath pos (m:moves)    =
    -- doMove on m, and recurse on remainder
    followPath (doMove pos m) moves


doMove :: Position -> Move -> Position
doMove pos move = pos -- boilerplate!

    where
        rotatedPos = over (direction) (rotateDir (fst move)) (pos)


rotateDir :: Rotation -> Direction -> Direction
rotateDir L pos =
    case (pos) of
        North   -> West
        West    -> South
        South   -> East
        East    -> North

rotateDir R pos =
    case (pos) of
        North   -> East
        East    -> South
        South   -> West
        West    -> North


-- Step 3: Find the shortest path to those coordinates

-- Step 4: Print it's length.


-- Apparently main needs to be last with TemplateHaskell?
main = do
    rawInput <- readFile "./input"
    putStrLn $ show $ followPath starting_pos (parseInput rawInput)


