#! /usr/bin/env -S stack runhaskell
import Data.Char (toUpper)

data Command = Forward Int | Up Int | Down Int deriving (Show, Read)

part1 :: [Command] -> Int
part1 = go 0 0
    where
        go d p [] = d * p
        go d p ((Forward x):xs) = go d (p+x) xs
        go d p ((Up x):xs) = go (d-x) p xs
        go d p ((Down x):xs) = go (d+x) p xs

part2 :: [Command] -> Int
part2 = go 0 0
    where
        go d p a [] = d * p
        go d p a ((Forward x):xs) = go (d+a*x) (p+x) a xs
        go d p a ((Up x):xs) = go d p (a-x) xs
        go d p a ((Down x):xs) = go d p (a+x) xs

main :: IO()
main = do
    f <- readFile("2.in")
    let input = map read . map (\(x:xs) -> (toUpper x) : xs) $ lines f :: [Command]
    putStr "Solution for part 1: "
    putStrLn . show $ part1 input
    putStr "Solution for part 2: "
    putStrLn . show $ part2 input

-- Solution part 1: 1804520
-- Solution part 2: 1971095320
