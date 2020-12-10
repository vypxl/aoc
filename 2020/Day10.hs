#! /usr/bin/env -S stack runhaskell
import Data.List

part1 :: [Int] -> Int
part1 xs = n1 * n2
    where 
        sorted = sort xs -- sort the list (obvious valid configuration)
        values = 0 : sorted ++ [last sorted + 3] -- add 0 and own adapter
        (n1, n2) = differences (0, 0) values -- compute number of 1 and 3 jolt differences
        differences v (_:[]) = v
        differences (ones, threes) (a:b:xs) = differences (case (b - a) of
            1 -> (ones + 1, threes)
            3 -> (ones, threes + 1)
            _ -> (ones, threes)) (b:xs)

part2 :: [Int] -> Int
part2 xs = product . map trib . lengths . reverse . optionals $ sort xs

optionals :: [Int] -> [Int]
optionals = run [] False
    where
        run acc carry (a:b:xs) = if b - a == 3 then
                                    run acc True (b:xs)
                                else (if carry then 
                                        run acc False (b:xs) 
                                        else run (a:acc) False (b:xs)
                                     )
        run acc _ _ = acc

trib :: Int -> Int
trib n = doTrib 2 4 7 1 n
    where doTrib a b c k n = if k >= n then a else doTrib b c (a + b + c) (k + 1) n

lengths :: [Int] -> [Int]
lengths xs = run [1] xs
    where
        run (t:acc) (a:b:xs) = if b - a == 1 then run (t + 1 : acc) (b : xs) else run (1 : t : acc) (b : xs)
        run acc _ = acc

main :: IO()
main = do
    f <- readFile("10.in")
    let input = map read . lines $ f
    putStr "Solution for part 1: "
    putStrLn . show $ part1 input
    putStr "Solution for part 2: "
    putStrLn . show $ part2 input

-- Solution part 1: 2450
-- Solution part 2: 32396521357312
