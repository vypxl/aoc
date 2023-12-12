#! /usr/bin/env -S runhaskell
import Control.Arrow ((***))
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

import Util

type Line = (String, [Int])
type Input = [Line]
type Output = Int

parse :: String -> Maybe Input
parse input = Just . catMaybes $ map parseLine (lines input)
  where
    parseLine :: String -> Maybe (String, [Int])
    parseLine line =
      case splitOn " " line of
        [str, nums] -> Just (str, map read $ splitOn "," nums)
        _           -> Nothing

counts :: String -> [Int] -> Int
counts = curry . memo $ uncurry counts'
counts' :: String -> [Int] -> Int
counts' [] [] = 1
counts' [] [0] = 1
counts' [] _ = 0
counts' ('#':l) ([]) = 0
counts' ('#':l) (0:gs) = 0
counts' ('#':l) (g:gs) = counts l (-((abs g) - 1):gs)
counts' ('.':l) (0:gs) = counts l gs
counts' ('.':l) (g:gs) = if g < 0 then 0 else counts l (g:gs)
counts' ('.':l) ([]) = counts l []
counts' ('?':l) gs = counts ('#':l) gs + counts ('.':l) gs

part1 :: Input -> Output
part1 = sum . map (uncurry counts)

part2 :: Input -> Output
part2 = part1 . map ((intercalate "?" . replicate 5) *** (concat . replicate 5))

main :: IO()
main = do
    f <- readFile("12.in")
    let input = parse f
    case input of
      Nothing -> putStrLn "Unable to read input."
      Just i -> do
        putStr "Solution for part 1: "
        putStrLn . show $ part1 i
        putStr "Solution for part 2: "
        putStrLn . show $ part2 i

-- Solution part 1: 6981
-- Solution part 2:
