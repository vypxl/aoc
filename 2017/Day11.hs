import           Data.List
import           Data.List.Split
import           System.IO.Unsafe

data Vec = Vec Float Float deriving (Show)

directions = [Vec 0 1,Vec 0 (-1),Vec (-1) 0.5,Vec 1 (-0.5),Vec (-1) (-0.5),Vec 1 0.5]

addVec :: Vec -> Vec -> Vec
addVec (Vec x1 y1) (Vec x2 y2) = Vec (x1 + x2) (y1 + y2)

subVec :: Vec -> Vec -> Vec
subVec (Vec x1 y1) (Vec x2 y2) = Vec (x1 - x2) (y1 - y2)

sumVec :: [Vec] -> Vec
sumVec = foldl (addVec) (Vec 0 0)

sumsVec :: [Vec] -> [Vec]
sumsVec xs = map (\i -> sumVec $ take i xs) [1..length xs]

heurDist :: Vec -> Float
heurDist (Vec x y) = abs x + abs y

dist :: Integral a => Vec -> a
dist v@(Vec x y)
  | heurDist best > 0 = 1 + (dist best)
  | otherwise = 1
  where
    best = foldl (\acc v -> if heurDist acc > heurDist v then v else acc) v $ map (addVec v) directions

main :: IO()
main =
  let
    vecs = map (\e -> dirToVec e) $ splitOn "," $ unsafePerformIO . readFile $ "input11.txt"
    sum = sumVec vecs
    dists = map (dist) $ sumsVec vecs
  in do
    print $ dist sum -- Part 1
    print . maximum $ dists -- Part 2

dirToVec :: String -> Vec
dirToVec dir
  | dir == "n"  = Vec 0 1
  | dir == "s"  = Vec 0 (-1)
  | dir == "nw" = Vec (-1) 0.5
  | dir == "se" = Vec 1 (-0.5)
  | dir == "sw" = Vec (-1) (-0.5)
  | dir == "ne" = Vec 1 0.5

