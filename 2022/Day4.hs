#! /usr/bin/env -S stack runhaskell
import Data.List
import Data.List.Split
import Data.Char

type Input = [([Int], [Int])]
type Output = Int

tuple2 :: [a] -> (a, a)
tuple2 [x, y] = (x, y)

parse :: String -> Input
parse = map (tuple2 . map (uncurry enumFromTo . tuple2 . map read . splitOn "-") . splitOn ",") . lines

issubset :: Eq a => [a] -> [a] -> Bool
issubset = (null . ) . (\\)

count :: (a -> Bool) -> [a] -> Int
count = (length . ) . filter

part1 :: Input -> Output
part1 = count $ or . (uncurry <$> ([id, flip] <*> pure issubset) <*>) . pure

part2 :: Input -> Output
part2 = count $ not . null . uncurry intersect

main :: IO()
main = do
    f <- readFile("4.in")
    let input = parse f
    putStr "Solution for part 1: "
    putStrLn . show $ part1 input
    putStr "Solution for part 2: "
    putStrLn . show $ part2 input

-- Solution part 1: 450
-- Solution part 2: 837
