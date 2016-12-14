-- Advent of Code day 1
-- Find the shortest path equivalent to a given sequence of movements
-- and print the length of it, using "taxicab geometry"

main = do
    rawInput <- readFile "./input"
    putStrLn $ show $ parseInput rawInput


-- Step 1: Parse the input into a useful form

--parseInput :: String -> [(Char, Int)]
parseInput rawInput =
    map (parseMove) moveList
    where
        moveList   = words $ filter (/= ',') rawInput

parseMove :: String -> (Char, Int)
parseMove (dir:num) =
    (dir, read num :: Int)

-- Step 2: Find the resulting coordinates of the given path


-- Step 3: Find the shortest path to those coordinates

-- Step 4: Print it's length.
