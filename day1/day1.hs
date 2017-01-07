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

    -- Starting a new move?
    | (snd m) == 0  = followPath (rotatedPos:history) (moves)

    -- is this a location we've been before? If so, return the answer
    | or $ map (isSameLocation forward) history = forward

    -- Are we still in the middle of a move?
    | (snd m) > 0   = followPath (forward:history) ((smallerMove):moves)

    where
        --newHistory  = newPos:history
        forward     = (doMove currentPos m)
        currentPos  = head history
        rotatedPos  = over (direction) (rotateDir (fst (head moves) )) (currentPos)
        smallerMove = over (_2) (subtract 1) m

-- Compare Positions, ignoring direction
isSameLocation :: Position -> Position -> Bool
isSameLocation a b =
    (a^.x == b^.x) && (a^.y == b^.y)


doMove :: Position -> Move -> Position
doMove pos move =

    addPos pos moveForward
    where
        dist        = snd move
        moveForward = case (pos^.direction) of
            North   -> ( 0, 1)
            West    -> (-1, 0)
            South   -> ( 0,-1)
            East    -> ( 1, 0)

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
    -- We cons an empty move on the start because I'm bad at this
    putStrLn $ show $ pathLength $ followPath [starting_pos] ((R,0):(parseInput rawInput))


