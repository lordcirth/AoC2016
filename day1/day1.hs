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


-- Step 2: Find the resulting coordinates of the given path: Done
-- new version: get list of history, apply the move to the last one, compare
followPath :: [Position] -> [Move] -> Position

-- main recursive function
followPath history (m:moves)
    -- is this a location we've been before?
    | or $ map (isSameLocation newPos) history = newPos

    -- Otherwise, recurse
    | otherwise = followPath newHistory moves
    -- doMove on m, and recurse on remainder
    where
        newHistory  = newPos:history
        newPos      = (doMove currentPos m)
        currentPos  = head history

-- Compare Positions, ignoring direction
isSameLocation :: Position -> Position -> Bool
isSameLocation a b =
    (a^.x == b^.x) && (a^.y == b^.y)


doMove :: Position -> Move -> Position
doMove pos move =
    addPos rotatedPos moveForward
    where
        rotatedPos  = over (direction) (rotateDir (fst move)) (pos)
        dist        = snd move
        moveForward = case (rotatedPos^.direction) of
            North   -> ( 0, dist)
            West    -> (-dist, 0)
            South   -> ( 0,-dist)
            East    -> ( dist, 0)

addPos :: Position -> (Int, Int) -> Position
addPos pos (x2, y2) =
    Position (x1+x2) (y1+y2) dir
    where
        dir = pos^.direction
        x1  = pos^.x
        y1  = pos^.y


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
pathLength :: Position -> Int
pathLength pos =
    abs (pos^.x) + abs (pos^.y)


-- Step 4: Print it's length.
-- Apparently main needs to be last with TemplateHaskell?
main = do
    rawInput <- readFile "./input"
    --putStrLn $ show $ pathLength $ followPath starting_pos (parseInput rawInput)
    putStrLn $ show $ pathLength $ followPath [starting_pos] (parseInput rawInput)


