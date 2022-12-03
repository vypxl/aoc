#! /usr/bin/env -S stack runhaskell
import Data.List
import Data.List.Split
import Data.Char

type Input = String
type Output = Int

parse :: String -> Input
parse = id

part1 :: Int -> Ouptut
part1 _ = 0

part2 :: Int -> Output
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
