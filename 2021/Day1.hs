#! /usr/bin/env -S stack runhaskell
import Data.List

part1 :: [Int] -> Int
part1 [] = 0
part1 [_] = 0
part1 (x:y:xs) = (if y > x then 1 else 0) + part1 (y:xs)

part2 :: [Int] -> Int
part2 = part1 . slide
    where 
        slide [] = []
        slide [_] = []
        slide [_, _] = []
        slide (x:y:z:xs) = (x + y + z) : slide (y:z:xs)

main :: IO()
main = do
    f <- readFile("1.in")
    let input = map read $ lines f
    putStr "Solution for part 1: "
    putStrLn . show $ part1 input
    putStr "Solution for part 2: "
    putStrLn . show $ part2 input

-- Solution part 1: 1121
-- Solution part 2: 1065
