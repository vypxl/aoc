import Data.List
import Data.List.Split
import System.IO.Unsafe

data Action = Spin Int | Exchange Int Int | Partner Char Char deriving Show

parse :: String -> Action
parse (x:xs)
    | x == 's' = Spin (read xs :: Int)
    | x == 'x' = let split = splitOn "/" xs in Exchange (read (split!!0) :: Int) $ (read (split!!1) :: Int)
    | x == 'p' = let split = splitOn "/" xs in Partner (head $ head split) $ last $ last split

input = map parse $ splitOn "," $ unsafePerformIO $ readFile "input16.txt"

main :: IO()
main = do
    print $ p1 ['a'..'p'] input
    print $ p2 input

performAction :: String -> Action -> String
performAction str (Spin x) = (drop (length str - x) str) ++ (take (length str - x) str)
performAction str (Exchange x y) = [if i == x then str!!y else if i == y then str!!x else str!!i | i <- [0..length str - 1]]
performAction str (Partner x y) = [if c == x then y else if c == y then x else c | c <- str]

p1 :: String -> [Action] -> String
p1 str xs = foldl performAction str xs

p2 :: [Action] -> String
p2 xs = cycle !! (1000000000 `mod` (cycleLength+1))
    where
        unjust (Just a) = a
        inf = scanl (\acc _ -> p1 acc xs) (p1 ['a'..'p'] xs) [1..]
        cycleLength = unjust $ elemIndex "abcdefghijklmnop" $ inf
        cycle = take (cycleLength+1) $ ['a'..'p'] : inf
