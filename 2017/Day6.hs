import           Data.List
import           Data.List.Split

input = "10 3 15 10 5 15 5 15 9 2 5 8 5 2 3 6"
-- input = "0 2 7 0"

main :: IO()
main = print $ f (map read (splitOn " " input) :: [Int]) [] 0

unjust (Just x) = x

f :: [Int] -> [[Int]] -> Int -> (Int, Int)
f banks seen steps =
  if banks `elem` seen then (steps, steps - unjust (elemIndex banks $ reverse seen))
    else f
      [(if i == toDistr then 0 else banks!!i) + length
          [1 | y <- [1..banks!!toDistr], ((y + toDistr) `mod` length banks) == i]
        | i <- [0..(length banks - 1)]]
      (banks : seen) (steps + 1)
    where
      toDistr = unjust $ elemIndex (maximum banks) banks
