#! /usr/bin/env -S stack runhaskell
import qualified Data.Set as Set
import Debug.Trace (trace)

type Game = ([Int], [Int])
type Result = Either Int Int

-- mock = "Player 1:\n\
-- \9\n\
-- \2\n\
-- \6\n\
-- \3\n\
-- \1\n\
-- \\n\
-- \Player 2:\n\
-- \5\n\
-- \8\n\
-- \4\n\
-- \7\n\
-- \10\n\
-- \"

mock = "Player 1:\n43\n19\n\nPlayer 2:\n2\n29\n14\n"

parse :: [String] -> Game
parse xs = (map read a, map read b)
    where
        [a, b] = splitOn (`elem` ["", "Player 1:", "Player 2:"]) xs

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
    | f x = splitOn f xs
    | otherwise = let (h,t) = break f l in h:(splitOn f t)

either :: Either a a -> a
either (Left a) = a
either (Right a) = a

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

score :: [Int] -> Int
score = go 1 . reverse
    where go k (x:xs) = k * x + (go (k+1) xs)
          go _ [] = 0

part1 :: Game -> Int
part1 (x:xs, y:ys) = if x > y then part1 (xs ++ [x, y], ys) else part1 (xs, ys ++ [y, x])
part1 (xs, []) = score xs
part1 ([], ys) = score ys

part2 :: Game -> Int
part2 game = Main.either $ play2 game Set.empty

play2 :: Game -> Set.Set Game -> Result
play2 game@(x:xs, y:ys) seen
    | Set.member game seen = Left $ score xs
    | x <= length xs && y <= length ys = if isLeft $ play2 (take x xs, take y ys) Set.empty then aWon else bWon
    | otherwise = if x > y then aWon else bWon
    where
        newSeen = Set.insert game seen
        aWon = play2 (xs ++ [x, y], ys) newSeen
        bWon = play2 (xs, ys ++ [y, x]) newSeen
        -- play2' (xs, ys) = if maximum xs > maximum ys then Left xs else Right ys
play2 (xs, []) _ = Left $ score xs
play2 ([], ys) _ = Right $ score ys

main :: IO()
main = do
    f <- readFile("22.in")
    let input = parse . lines $ f
    -- let input = parse . lines $ mock
    print input
    putStr "Solution for part 1: "
    putStrLn . show $ part1 input
    putStr "Solution for part 2: "
    putStrLn . show $ part2 input

-- Solution part 1: 31455
-- Solution part 2: 32528
