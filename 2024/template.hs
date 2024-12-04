#! /usr/bin/env -S runhaskell
import Data.List
import Data.List.Split
import Data.Char

import Util

type Input = String
type Output = Int

parse :: String -> Maybe Input
parse = Just

part1 :: Input -> Output
part1 _ = 0

part2 :: Input -> Output
part2 _ = 1

main :: IO()
main = do
    f <- getContents
    let input = parse f
    case input of
      Nothing -> putStrLn "Unable to read input."
      Just i -> do
        putStr "Part1: "
        putStrLn . show $ part1 i
        -- putStr "Part2: "
        -- putStrLn . show $ part2 i
