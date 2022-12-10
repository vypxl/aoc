#! /usr/bin/env -S runhaskell
import Data.Ix (inRange)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Text.Read (readMaybe)

import Util

data Instr = Noop | Addx Int deriving (Show, Read)
data CPU = CPU { cycl :: Int, x :: Int, strength :: Int, screen :: String }
type Input = [Instr]
type Output = (Int, String)

parse :: String -> Maybe Input
parse = mapM (readMaybe . capitalize) . lines

solve :: Input -> Output
solve xs = let CPU { strength, screen } = foldl (flip doInstr) (CPU 1 1 0 "") xs in (strength, intercalate "\n" . chunksOf 40 $ reverse screen)
  where
    doCycle delta st@(CPU cycl x strength screen) = st {
      cycl = cycl + 1,
      x = x + delta,
      strength = (40 `divides` (cycl + 20) ? (cycl * x) $ 0) + strength,
      screen = (inRange (x, x + 2) ((cycl - 1) `mod` 40 + 1) ? '█' $ ' ') : screen
    }
    doInstr Noop = doCycle 0
    doInstr (Addx delta) = doCycle delta . doCycle 0

main :: IO()
main = do
    f <- readFile("10.in")
    let input = parse f
    case input of
      Nothing -> putStrLn "Unable to read input."
      Just i -> do
        let (p1, p2) = solve i
        putStr "Solution for part 1: "
        putStrLn . show $ p1
        putStr "Solution for part 2:\n"
        putStrLn p2

-- Solution part 1: 14540
-- Solution part 2: EHZFZHCZ
