-- Advent of Code day 1
-- Find the shortest path equivalent to a given sequence of movements
-- and print the length of it, using "taxicab geometry"

main = do
    rawInput <- readFile "./input"
    putStrLn $ show $ followPath (0,0) (parseInput rawInput)


type Move = (Char, Int)

data Direction  = North | South | East | West
data Position   = Position { x,y :: Int, dir :: Direction }

starting_pos = Position {x = 0, y = 0, dir = North }

-- Step 1: Parse the input into a useful form: Done

parseInput :: String -> [Move]
parseInput rawInput =
    map (parseMove) moveList
    where
        moveList   = words $ filter (/= ',') rawInput


parseMove :: String -> Move
parseMove (dir:num) =
    (dir, read num :: Int)


-- Step 2: Find the resulting coordinates of the given path

followPath :: Position -> [Move] -> Position

-- Recursive base case
followPath pos []           = pos

-- main recursive function
followPath pos (m:moves)    =
    followPath move dir pos m


doMove :: Position -> Move -> Position
doMove pos move = pos -- boilerplate!


-- rotateDir


-- Step 3: Find the shortest path to those coordinates

-- Step 4: Print it's length.
