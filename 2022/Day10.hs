#! /usr/bin/env -S runhaskell
import Data.Ix (inRange)
import Data.List (intercalate)
import Data.List.Split (chunksOf, splitOn)
import Data.Char (ord)

import Criterion.Main

import Util

data Instr = Noop | Addx Int deriving (Show)
data CPU = CPU { cycl :: Int, x :: Int, strength :: Int, screen :: String }

readInstr :: String -> Instr
readInstr s = case drop 5 s of [] -> Noop; r -> Addx (readI r)

readC :: Char -> Int
readC c = ord c - ord '0'

readI :: String -> Int
readI ('-':xs) = -(readI xs)
readI [x, y] = (readC x) * 10 + (readC y)
readI [c] = readC c

parse :: String -> [Instr]
parse = map readInstr . splitOn "\n"

solve :: [Instr] -> (Int, String)
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

main :: IO ()
main = do
  let fname = "10.in"
  f <- readFile fname
  let input = parse f
  defaultMain [
    bgroup "Day 10" [ bench "Full" $ nfIO $ (\f -> readFile f >>= (return . solve . parse)) fname
                    , bench "Parse + Solve, no IO" $ nf (solve . parse) f
                    , bench "Solve only" $ nf solve input
                    , bench "read -42" $ nf (readI :: (String -> Int)) "-42"
                    ]
    ]
  main'

main' :: IO()
main' = do
    f <- readFile("10.in")
    let input = parse f
    print input
    let (p1, p2) = solve input
    putStr "Solution for part 1: "
    putStrLn . show $ p1
    putStr "Solution for part 2:\n"
    putStrLn p2

-- Solution part 1: 14540
-- Solution part 2: EHZFZHCZ
