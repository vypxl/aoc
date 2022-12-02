#! /usr/bin/env -S stack runhaskell
import Data.Char

parse :: String -> [(Int, Int)]
parse = map (\[a, _, b] -> (ord(a) - ord('A'), ord(b) - ord('X'))) . lines

part1 :: [(Int, Int)] -> Int
part1 = sum . map (\(a, b) -> 1 + b + ((b - a + 1) `mod` 3) * 3)

part2 :: [(Int, Int)] -> Int
part2 = part1 . map (\(a, b) -> (a, (a + b - 1) `mod` 3))

main :: IO()
main = do
    f <- readFile("2.in")
    let input = parse f
    putStr "Solution for part 1: "
    putStrLn . show $ part1 input
    putStr "Solution for part 2: "
    putStrLn . show $ part2 input

-- Solution part 1: 11449
-- Solution part 2: 13187
