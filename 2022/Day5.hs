#! /usr/bin/env -S runhaskell
import Control.Monad ((<=<), filterM)
import Data.Array
import Data.Char (isAlpha)
import Data.List
import Data.List.Split
import Data.Maybe (isJust, mapMaybe)
import Text.Read (readMaybe)

import Util

type Stack = String
type Command = (Int, Int, Int)
type Stacks = Array Int Stack

data Input = Input Stacks [Command] deriving (Show)
type Output = String

parse :: String -> Maybe Input
parse s = fmap (Input $ listArray (1,length stacks) stacks) $ parseCommands b
  where
    [a, b] = superlines s
    parseCommands = mapM (tuple3 . mapMaybe readMaybe . splitOn " ") . lines
    parseStacks = filter (not . null) . map (filter isAlpha . trim) . transpose . lines
    stacks = parseStacks a

move :: Bool -> Stacks -> Command -> Stacks
move rev stacks (amount, from, to) = stacks // [(from, stackA), (to, stackB)]
 where
  toMove = (if rev then reverse else id) . take amount $ stacks ! from
  stackA = drop amount $ stacks ! from
  stackB = toMove ++ stacks ! to

solve :: Bool -> Input -> Output
solve isPart1 (Input stacks commands) = map head . elems $ foldl (move isPart1) stacks commands

part1 :: Input -> Output
part1 = solve True

part2 :: Input -> Output
part2 = solve False

main :: IO()
main = do
    f <- readFile("5.in")
    let input = parse f
    case input of
      Nothing -> putStrLn "Unable to read input."
      Just i -> do
        putStr "Solution for part 1: "
        putStrLn . show $ part1 i
        putStr "Solution for part 2: "
        putStrLn . show $ part2 i

-- Solution part 1: SHQWSRBDL
-- Solution part 2: CDTQZHBRS
