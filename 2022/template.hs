#! /usr/bin/env -S stack runhaskell
import Data.List

parse :: String -> String
parse = id

part1 :: Int -> Int
part1 _ = 0

part2 :: Int -> Int
part2 _ = 1

main :: IO()
main = do
    f <- readFile("_DAY_.in")
    let input = parse f
    putStr "Solution for part 1: "
    putStrLn . show $ part1 input
    putStr "Solution for part 2: "
    putStrLn . show $ part2 input

-- Solution part 1:
-- Solution part 2:
