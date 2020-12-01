#! /usr/bin/env -S stack runhaskell
import Data.List

part1 :: Int -> Int
part1 _ = 0

part2 :: Int -> Int
part2 _ = 1

main :: IO()
main = do
    f <- readFile(".in")
    let input = 0
    putStrLn "Solution for part 1:"
    print $ part1 input
    putStrLn "Solution for part 2:"
    print $ part2 input

-- Solution part 1: 
-- Solution part 2: 
