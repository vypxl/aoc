#! /usr/bin/env -S runhaskell
import Data.Ix (inRange)
import Data.Char (ord)

import qualified Data.ByteString.Lazy.Char8 as B
import Prelude hiding (readFile)

import Criterion.Main

import Util

data Instr = Noop | Addx Int deriving (Show)
data CPU = CPU { cycl :: Int, x :: Int, offset :: Int, strength :: Int, screen :: String }

readC :: Char -> Int
readC c = ord c - ord '0'

parse :: String -> [Instr]
parse = fst . go
  where
    go ('n':_:_:_:_:xs) = let (is, rest) = go xs in (Noop:is, rest)
    go (_:_:_:_:_:c:'\n':xs) = let (is, rest) = go xs in ((Addx $ readC c):is, xs)
    go (_:_:_:_:_:'-':c:'\n':xs) = let (is, rest) = go xs in ((Addx $ - readC c):is, xs)
    go (_:_:_:_:_:'-':x:y:_:xs) = let (is, rest) = go xs in ((Addx $ -((readC x) * 10 + (readC y))):is, rest)
    go (_:_:_:_:_:x:y:_:xs) = let (is, rest) = go xs in ((Addx $ (readC x) * 10 + (readC y)):is, rest)
    go [] = ([], [])

solve :: [Instr] -> (Int, String)
solve xs = let CPU { strength, screen } = foldl (flip doInstr) (CPU 1 1 1 0 "") xs in (strength, reverse screen)
  where
    doCycle delta st@(CPU cycl x offset strength screen) = st {
      cycl = cycl + 1,
      offset = 40 `divides` cycl ? 1 $ offset + 1,
      x = x + delta,
      strength = (cycl * x * fromEnum (40 `divides` (cycl + 20))) + strength,
      screen = let c = inRange (x, x+2) offset ? '█' $ ' ' in offset == 40 ? '\n':c:screen $ c:screen
    }
    doInstr Noop = doCycle 0
    doInstr (Addx delta) = doCycle delta . doCycle 0

main :: IO ()
main = do
  let fname = "10.in"
  f <- B.readFile fname
  let input = parse $ B.unpack f
  main'
  defaultMain [
    bgroup "Day 10" [ bench "Full" $ nfIO $ (\f -> B.readFile f >>= (return . solve . parse . B.unpack)) fname
                    , bench "Parse + Solve, no IO" $ nf (solve . parse . B.unpack) f
                    , bench "Solve only" $ nf solve input
                    ]
    ]

main' :: IO()
main' = do
    f <- B.readFile "10.in"
    let input = parse $ B.unpack f
    let (p1, p2) = solve input
    putStr "Solution for part 1: "
    putStrLn . show $ p1
    putStr "Solution for part 2:\n"
    putStrLn p2

-- Solution part 1: 14540
-- Solution part 2: EHZFZHCZ
