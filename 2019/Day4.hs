#! /usr/bin/env -S stack runhaskell
import Data.List
import Data.Maybe

bothParts :: (Int, Int) -> [Int]
bothParts (start, end) = map (length . (flip filter) common) [multiple, double] 
        where
            increasingDigits xs = sort xs == xs
            multiple   = any (>1)  . map length . group
            double     = any (==2) . map length . group
            common     = filter increasingDigits . map show $ [start..end]

split :: String -> String -> [String]
split _ "" = []
split sep str = let (left, right) = splitAt idx str in left : split sep (drop (length sep) right)
        where idx = fromMaybe (length str) $ findIndex (isPrefixOf sep) (tails str)

parse :: String -> (Int, Int)
parse s = let (a:b:[]) = map read $ split "-" s in (a, b)

main :: IO()
main = do
    f <- readFile("4.in")
    let input = parse f
    let (part1:part2:_) = bothParts input
    putStrLn "Solution for part 1:"
    print part1
    putStrLn "Solution for part 2:"
    print part2

-- Solution part 1: 925
-- Solution part 2: 607
