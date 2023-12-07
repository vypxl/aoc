#! /usr/bin/env -S runhaskell
import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe

import Util

type Hand = [Int]
type Input = [(Hand, Int)]
type Output = Int

parse_card :: Int -> Char -> Int
parse_card 1 c = fromJust $ elemIndex c "23456789TJQKA"
parse_card 2 c = fromJust $ elemIndex c "J23456789TQKA"

parse :: Int -> String -> Input
parse part = map (f . splitOn " ") . lines
  where f [a, b] = (map (parse_card part) a, read b)

counts :: Eq a => [a] -> [(a, Int)]
counts xs = reverse $ sortOn snd [(x, count (==x) xs) | x <- nub xs]

jokers :: [(Int, Int)] -> [(Int, Int)]
jokers xs = mx:tail rest
  where
    (_, joker_count) = maybe (0, 0) id $ find ((==0) . fst) xs
    rest = xs \\ [(0, joker_count)]
    (mx_card, mx_count) = if length rest == 0 then (1, 0) else head rest
    mx = (mx_card, mx_count + joker_count)

typ :: Int -> Hand -> Int
typ part = go . (if part == 2 then jokers else id) . counts
  where
    go ((_, k):xs)
      | k == 5 = 7
      | k == 4 = 6
      | k == 3 = if snd (head xs) == 2 then 5 else 4
      | k == 2 = if snd (head xs) == 2 then 3 else 2
      | otherwise = 1

compare_hands :: Int -> Hand -> Hand -> Ordering
compare_hands part a b = compare (typ part a) (typ part b) <> compare a b

solve :: Int -> String -> Output
solve part = sum . map (uncurry (*)) . zip [1..] . map snd . sortBy (\a b -> compare_hands part (fst a) (fst b)) . parse part

part1 :: String -> Output
part1 = solve 1

part2 :: String -> Output
part2 = solve 2

main :: IO()
main = do
    f <- readFile("7.in")
    let input = f
    putStr "Solution for part 1: "
    putStrLn . show $ part1 input
    putStr "Solution for part 2: "
    putStrLn . show $ part2 input

-- Solution part 1: 250058342
-- Solution part 2: 250506580
