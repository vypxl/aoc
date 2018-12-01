import Data.List
import Data.List.Split

main :: IO()
main = do
   -- let input = "<-6,0,0>;< 3,0,0>;< 0,0,0>\n<-4,0,0>;< 2,0,0>;< 0,0,0>\n<-2,0,0>;< 1,0,0>;< 0,0,0>\n< 3,0,0>;<-1,0,0>;< 0,0,0>"
   input <- readFile "input20.txt"
   let parsed = parse $ lines input
   -- print parsed
   -- print $ p1 parsed 2000
   print $ p2 parsed 100 --9223372036854775807
   return ()

data Vec = Vec Int Int Int deriving (Show, Eq)

addVec :: Vec -> Vec -> Vec
addVec (Vec x1 y1 z1) (Vec x2 y2 z2) = Vec (x1 + x2) (y1 + y2) (z1 + z2)

manDistV :: Vec -> Int
manDistV (Vec x y z) = abs x + abs y + abs z

data Particle = Particle Vec Vec Vec deriving (Show)

manDistP :: Particle -> Int
manDistP (Particle pos _ _) = manDistV pos

tick :: Particle -> Particle
tick (Particle pos vel acc) = Particle (addVec newVel pos) newVel acc
    where newVel = addVec vel acc

parse :: [String] -> [Particle]
parse = map (particle . parseV . triml)
        where triml l = map (tail . init) (splitOn ";" l)
              parseV strs = [Vec (read (head nums) :: Int) (read (nums!!1) :: Int) (read (last nums) :: Int) | str <- strs, let nums = splitOn "," str]
              particle vecs = Particle (head vecs) (vecs!!1) (last vecs)

p1 :: [Particle] -> Int -> (Int, Particle)
p1 particles iters = foldl1 (\acc new -> if manDistP (snd new) < manDistP (snd acc) then new else acc) after
                     where after = zip [0..] $ foldl (\acc _ -> map tick acc) particles [1..iters]

p2 :: [Particle] -> Int -> [Int]
p2 particles iters = map length $ scanl (\acc _ -> concat $ filter (\group -> 1 == length group) $ groupBy (\(Particle pos1 _ _) (Particle pos2 _ _) -> pos1 == pos2) $
                     sortBy (\left right -> compare (manDistP left) $ manDistP right) $ map tick acc) particles [1..iters]

