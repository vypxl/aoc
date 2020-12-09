#! /usr/bin/env -S stack runhaskell
import Data.List

part1 :: [Int] -> Int
part1 [] = -1
part1 xs = if elem el possible then part1 (tail xs) else el
    where pre = take 25 xs
          possible = [(+)] <*> pre <*> pre
          post = drop 25 xs
          el = head post

part2 :: [Int] -> Int
part2 xs = (foldr1 max cset) + (foldr1 min cset)
    where cset = findCset (part1 xs) xs

findCset :: Int -> [Int] -> [Int]
findCset _ [] = []
findCset n xs = cset . elemIndex n . scanl1 (+) $ xs
    where cset Nothing = findCset n (tail xs)
          cset (Just i) = take i xs

main :: IO()
main = do
    f <- readFile("9.in")
    let input = map read . lines $  f
    putStr "Solution for part 1: "
    putStrLn . show $ part1 input
    putStr "Solution for part 2: "
    putStrLn . show $ part2 input

-- Solution part 1: 675280050
-- Solution part 2: 96081673
