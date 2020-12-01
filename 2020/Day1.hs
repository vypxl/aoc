#! /usr/bin/env -S stack runhaskell
part1 :: [Int] -> Int
part1 xs = head [x * y | x <- xs, y <- xs, x + y == 2020]

part2 :: [Int] -> Int
part2 xs = head [x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]

main :: IO()
main = do
    f <- readFile("1.in")
    let input = map read $ lines f
    putStr "Solution for part 1: "
    putStrLn . show $ part1 input
    putStr "Solution for part 2: "
    putStrLn . show $ part2 input

-- Solution part 1: 1007104
-- Solution part 2: 18847752
