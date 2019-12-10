#! /usr/bin/env -S stack runhaskell
import Data.List
import Data.Function (on)
import Data.Maybe (fromMaybe)

data Asteroid = Asteroid Float Float deriving (Show, Eq, Ord)

angle :: Asteroid -> Asteroid -> Float
angle (Asteroid x1 y1) (Asteroid x2 y2) = (if ang < 0 then ang + (pi * 2) else ang) * 180 / pi
    where ang = (pi / 2) + atan2 (y2 - y1) (x2 - x1)

dist :: Asteroid -> Asteroid -> Float
dist (Asteroid x1 y1) (Asteroid x2 y2) = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

countVisible :: Asteroid -> [Asteroid] -> Int
countVisible from = length . nubBy ((==) `on` (angle from)) . (delete from)

getBestStation :: [Asteroid] -> Asteroid
getBestStation xs = maximumBy (compare `on` ((flip countVisible) xs)) xs

getDestroyedOrder :: Asteroid -> [Asteroid] -> [Asteroid]
getDestroyedOrder station xs = order (-1) sorted
    where
        sorted = sort $ map (\a -> (angle station a, dist station a, a)) xs
        order _ [] = []
        order lastAng xs = ast : order ang (delete chosen xs)
            where
                chosen@(ang, _, ast) = fromMaybe (head xs) . find (\(a, _, _) -> a > lastAng) $ xs

part1 :: [Asteroid] -> Int
part1 xs = maximum $ map ((flip countVisible) xs) xs 

part2 :: [Asteroid] -> Int
part2 xs = let (Asteroid x y) = head . drop 199 . getDestroyedOrder (getBestStation xs) $ xs in round $ x * 100 + y

parse :: String -> [Asteroid]
parse s = concat [[Asteroid x y | (x, c) <- zip [0..] l, c == '#'] | (y, l) <- zip [0..] (lines s)]

main :: IO()
main = do
    f <- readFile("10.in")
    let input = parse f
    putStrLn "Solution for part 1:"
    print $ part1 input
    putStrLn "Solution for part 2:"
    print $ part2 input

-- Solution part 1: 280
-- Solution part 2: 706
