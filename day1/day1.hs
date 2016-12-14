-- Advent of Code day 1
-- Find the shortest path, and print the distance, equivalent to a given sequence of movements
-- Using "taxicab geometry"

-- First step: Parse the input into a useful form

main = do
    rawInput <- readFile "./input"
    putStrLn $ show $ parseInput rawInput

--parseInput :: String -> [(Char, Int)]
parseInput rawInput =
    map (parseMove) moveList
    where
        moveList   = words $ filter (/= ',') rawInput

parseMove :: String -> (Char, Int)
parseMove (dir:num) =
    (dir, read num :: Int)
