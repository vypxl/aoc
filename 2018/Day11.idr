module Main

import Data.String
import Data.List

maxByThird : Ord c => List (a, b, c) -> (a, b, c)
maxByThird = foldr1 (\(ax, ay, av), (x, y, v) => if v > av then (x, y, v) else (ax, ay, av))

cell : Integer -> Integer -> Integer -> Integer
cell x y serial = (mod (div ((((x + 10) * y) + serial) * (x + 10)) 100) 10) - 5

grid : Integer -> List (List Integer)
grid serial = [[cell x y serial | y <- [1..300]] | x <- [1..300]]

squareCell : Integer -> Integer -> Integer -> List (List Integer) -> Integer
squareCell size x y g = if (x + size - 1) > 300 || (y + size - 1) > 300 then 0 else
    sum
    . map (
        sum
        . List.take (fromIntegerNat size)
        . List.drop (fromIntegerNat (y - 1)) 
        )
    . List.take (fromIntegerNat size)
    . List.drop (fromIntegerNat (x - 1))
    $ g

square : Integer -> List (List Integer) -> (Integer, Integer, Integer)
square size g = maxByThird [(x, y, squareCell size x y g) | x <- [1..300 - (size - 2)], y <- [1..300 - (size - 2)]]

part1 : List (List Integer) -> (Integer, Integer, Integer)
part1 = square 3

part2 : List (List Integer) -> (Integer, Integer, Integer)
part2 g = maxByThird [square size g | size <- [1..10]]

main : IO()
main = do
    (Right str) <- readFile "11.in"
    let (Just serial) = String.parseInteger str
    let g = grid serial
    putStrLn "(x, (y, power)"
    printLn $ square 1 g
    printLn $ square 2 g
    putStrLn "Solution for part 1:"
    printLn $ square 3 g
    printLn $ square 4 g
    printLn $ square 5 g
    printLn $ square 6 g
    printLn $ square 7 g
    printLn $ square 8 g
    printLn $ square 9 g
    printLn $ square 10 g
    printLn $ square 11 g
    printLn $ square 12 g
    printLn $ square 13 g
    printLn $ square 14 g
    printLn $ square 15 g
    printLn $ square 16 g
    printLn $ square 17 g
    printLn $ square 18 g
    printLn $ square 19 g
    printLn $ square 20 g
    printLn $ square 21 g
    printLn $ square 22 g
    printLn $ square 23 g
    printLn $ square 24 g
    printLn $ square 25 g
    printLn $ square 26 g
    printLn $ square 27 g
    printLn $ square 28 g
    printLn $ square 29 g
    
    -- I do not want to optimize this right now, so... it works so it's good.
    -- You just take the biggest power, seems to be between 13 and 20 all the time.
    -- Starts at one, the size is the line number, not counting the info lines.

-- Solution part 1: 235,87
-- Solution part 2: 234,272,18
