#! /usr/bin/env -S stack runhaskell
import Data.List
import Data.List.Split

parse :: String -> [Int]
parse = reverse . sort . map (sum . map read . lines) . splitOn "\n\n"

part1 :: [Int] -> Int
part1 = head

part2 :: [Int] -> Int
part2 = sum . take 3

main :: IO()
main = do
    f <- readFile("1.in")
    let input = parse f
    putStr "Solution for part 1: "
    putStrLn . show $ part1 input
    putStr "Solution for part 2: "
    putStrLn . show $ part2 input

-- Solution part 1: 67450
-- Solution part 2: 199357
