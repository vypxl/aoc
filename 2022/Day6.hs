#! /usr/bin/env -S runhaskell
import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe (fromJust)

import Util

type Input = String
type Output = Int

parse :: String -> Maybe Input
parse = Just

solve :: Int -> Input -> Maybe Output
solve n = ((+n) <$>) . findIndex ((==) <$> nub <*> id) . map (take n) . tails

part1 :: Input -> Output
part1 = fromJust . solve 4

part2 :: Input -> Output
part2 = fromJust . solve 14

main :: IO()
main = do
    f <- readFile("6.in")
    let input = parse f
    case input of
      Nothing -> putStrLn "Unable to read input."
      Just i -> do
        putStr "Solution for part 1: "
        putStrLn . show $ part1 i
        putStr "Solution for part 2: "
        putStrLn . show $ part2 i

-- Solution part 1: 1598
-- Solution part 2: 2414
