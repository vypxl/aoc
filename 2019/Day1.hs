#! /usr/bin/env -S stack runhaskell

fuel :: Int -> Int
fuel = (+ (-2)) . (`div` 3)

fuelX :: Int -> Int
fuelX = (\m -> if m <= 0 then 0 else m + fuelX m) . fuel

main :: IO()
main = do
    f <- readFile("1.in")
    let input = map read $ lines f

    putStrLn "Solution for part 1:"
    print $ sum $ map fuel input
    putStrLn "Solution for part 2:"
    print $ sum $ map fuelX input

-- Solution part 1: 3297866
-- Solution part 2: 4943923
