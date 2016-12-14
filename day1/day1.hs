-- Advent of Code day 1
-- Find the shortest path equivalent to a given sequence of movements
-- and print the length of it, using "taxicab geometry"

main = do
    rawInput <- readFile "./input"
    putStrLn $ show $ followPath starting_pos (parseInput rawInput)


type Move = (Rotation, Int)

data Direction  = North | South | East | West deriving Show
data Position   = Position { x,y :: Int, direction :: Direction } deriving Show
data Rotation   = L | R deriving (Read, Show)

starting_pos = Position {x = 0, y = 0, direction = North }

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
--    followPath move pos m
    pos


doMove :: Position -> Move -> Position
doMove pos move = pos -- boilerplate!


rotateDir :: Direction -> Char -> Direction
rotateDir pos 'L' =
    case (pos) of
        North   -> West
        West    -> South
        South   -> East
        East    -> North

rotateDir pos 'R' =
    case (pos) of
        North   -> East
        East    -> South
        South   -> West
        West    -> North

rotateDir _ _ = error "Only rotations L and R are valid!"

-- Step 3: Find the shortest path to those coordinates

-- Step 4: Print it's length.
