#! /usr/bin/env -S stack runhaskell
data Inst = N Int | S Int | E Int | W Int | L Int | R Int | F Int deriving (Show, Read, Eq)
data Ship = Ship Int Int Int deriving (Show)
data Ship2 = Ship2 Int Int Int Int

dist :: Ship -> Int
dist (Ship x y _) = (abs x) + (abs y)
dist2 :: Ship2 -> Int
dist2 (Ship2 x y _ _) = (abs x) + (abs y)

move1 :: Inst -> Ship -> Ship
move1 (N k) (Ship x y f) = Ship x (y + k) f
move1 (S k) (Ship x y f) = Ship x (y - k) f
move1 (E k) (Ship x y f) = Ship (x + k) y f
move1 (W k) (Ship x y f) = Ship (x - k) y f
move1 (F k) (Ship x y f) = move1 (facing f $ k) (Ship x y f)
move1 (R k) (Ship x y f) = Ship x y (mod (f + k) 360)
move1 (L k) (Ship x y f) = Ship x y (mod (f + 360 - k) 360)

facing :: Int -> (Int -> Inst)
facing 0 = E
facing 90 = S
facing 180 = W
facing 270 = N

move2 :: Inst -> Ship2 -> Ship2
move2 (N k) (Ship2 x y u v) = Ship2 x y u (v + k)
move2 (S k) (Ship2 x y u v) = Ship2 x y u (v - k)
move2 (E k) (Ship2 x y u v) = Ship2 x y (u + k) v
move2 (W k) (Ship2 x y u v) = Ship2 x y (u - k) v
move2 (F k) (Ship2 x y u v) = Ship2 (x + u * k) (y + v * k) u v
move2 (R k) (Ship2 x y u v) = let (u', v') = rotate k u v in Ship2 x y u' v'
move2 (L k) (Ship2 x y u v) = let (u', v') = rotate (360-k) u v in Ship2 x y u' v'

rotate :: Int -> Int -> Int -> (Int, Int)
rotate 0   u v = (u,   v)
rotate 90  u v = (v,  -u)
rotate 180 u v = (-u, -v)
rotate 270 u v = (-v, u)

parse :: String -> Inst
parse = read . unwords . (\(a, b) -> [a, b]) . splitAt 1

part1 :: [Inst] -> Int
part1 = go (Ship 0 0 0)
    where
        go ship (inst:xs) = go (move1 inst ship) xs
        go ship [] = dist ship

part2 :: [Inst] -> Int
part2 = go (Ship2 0 0 10 1)
    where
        go ship (inst:xs) = go (move2 inst ship) xs
        go ship [] = dist2 ship

main :: IO()
main = do
    f <- readFile("12.in")
    let input = map parse $ lines f
    putStr "Solution for part 1: "
    putStrLn . show $ part1 input
    putStr "Solution for part 2: "
    putStrLn . show $ part2 input

-- Solution part 1: 2879
-- Solution part 2: 178986
