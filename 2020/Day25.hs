#! /usr/bin/env -S stack runhaskell

step :: Int -> Int -> Int
step subject num = (subject * num) `mod` 20201227

findloop :: Int -> Int -> Int -> Int
findloop subject num target = go subject num target 0
    where
        go s n t i
            | n == t = i
            | otherwise = go s (step s n) t (i + 1)

part1 :: [Int] -> Int
part1 [a, b] = (iterate (step b) 1) !! (findloop 7 1 a)

main :: IO()
main = do
    f <- readFile("25.in")
    let input = map read $ lines f
    putStr "Solution for part 1: "
    putStrLn . show $ part1 input

-- Solution part 1: 17032383
