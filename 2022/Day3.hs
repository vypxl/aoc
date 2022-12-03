#! /usr/bin/env -S stack runhaskell
import Data.List
import Data.List.Split
import Data.Char

type Input = [String]
type Output = Int

parse :: String -> Input
parse = lines

parts :: Int -> [a] -> [[a]]
parts k xs = chunksOf (length xs `div` k) xs

prio :: Char -> Int
prio c
  | isLower c = ord c - ord 'a' + 1
  | otherwise = ord c - ord 'A' + 27

sumCommonPriorities :: [[String]] -> Int
sumCommonPriorities = sum . map (prio . head . foldr1 intersect)

part1 :: Input -> Output
part1 = sumCommonPriorities . map (parts 2)

part2 :: Input -> Output
part2 = sumCommonPriorities . chunksOf 3

main :: IO()
main = do
    f <- readFile("3.in")
    let input = parse f
    putStr "Solution for part 1: "
    putStrLn . show $ part1 input
    putStr "Solution for part 2: "
    putStrLn . show $ part2 input

-- Solution part 1: 8139
-- Solution part 2: 2668
