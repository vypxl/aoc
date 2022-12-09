#! /usr/bin/env -S runhaskell
import Data.List
import Data.Maybe (fromMaybe)
import Control.Applicative

import Util

type Input = [[Int]]
type Output = Int

parse :: String -> Maybe Input
parse = grid

apply4Way :: ([a] -> [b]) -> (b -> b -> b) -> [[a]] -> [[b]]
apply4Way transformLine combineOp = ((map (uncurry combine) . ) . zip) <$> transformGrid <*> (transpose . transformGrid . transpose)
  where
    transformGrid = map (combine <$> transformLine <*> reverse . transformLine . reverse)
    combine = (map (uncurry combineOp) . ) . zip

part1 :: Input -> Output
part1 = sumBGrid . apply4Way transformLine (||)
  where transformLine = fst . foldr (\x (a, hi) -> ((hi < x):a, max hi x)) ([], -1)

part2 :: Input -> Output
part2 = (reducerGrid max) . apply4Way transformLine (*)
  where transformLine = map (\x -> fromMaybe ((length x) - 1) $ findIndex (>= head x) (head x - 1 : tail x)) . init . tails

main :: IO()
main = do
    f <- readFile("8.in")
    let input = parse f
    case input of
      Nothing -> putStrLn "Unable to read input."
      Just i -> do
        putStr "Solution for part 1: "
        putStrLn . show $ part1 i
        putStr "Solution for part 2: "
        putStrLn . show $ part2 i

-- Solution part 1: 1814
-- Solution part 2: 330786
