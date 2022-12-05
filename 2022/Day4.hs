#! /usr/bin/env -S runhaskell
import Data.List
import Data.List.Split
import Data.Char
import Control.Monad ((<=<))

import Util

type Input = [([Int], [Int])]
type Output = Int

parse :: String -> Maybe Input
parse = mapM (tuple2 <=< mapM ((uncurry enumFromTo <$>) . tuple2 . map read . splitOn "-") . splitOn ",") . lines

part1 :: Input -> Output
part1 = count $ or . (uncurry <$> ([id, flip] <*> pure issubset) <*>) . pure

part2 :: Input -> Output
part2 = count $ not . null . uncurry intersect

main :: IO()
main = do
    f <- readFile("4.in")
    let input = parse f
    case input of
      Nothing -> putStrLn "Unable to read input."
      Just i -> do
        putStr "Solution for part 1: "
        putStrLn . show $ part1 i
        putStr "Solution for part 2: "
        putStrLn . show $ part2 i

-- Solution part 1: 450
-- Solution part 2: 837
