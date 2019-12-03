#! /usr/bin/env -S stack runhaskell
import Prelude hiding (Right, Left)
import Data.List
import Data.Maybe
import qualified Data.Set as S

data Dir = Up | Down | Left | Right deriving (Show)
data Pos = Pos Int Int deriving (Show, Eq, Ord)
data Indexed a = Indexed Int a deriving (Show)

getIndex :: Indexed a -> Int
getIndex (Indexed i _) = i

unbox :: Indexed a -> a
unbox (Indexed _ x) = x

instance Eq a => Eq (Indexed a) where
    (Indexed _ a) == (Indexed _ b) = a == b
instance Ord a => Ord (Indexed a) where
    compare (Indexed _ a) (Indexed _ b) = compare a b

parseWire :: String -> [Dir]
parseWire str = concatMap parseDirStr $ split "," str
    where 
        parseDirStr ('R':n) = replicate (read n) Right
        parseDirStr ('L':n) = replicate (read n) Left
        parseDirStr ('U':n) = replicate (read n) Up
        parseDirStr ('D':n) = replicate (read n) Down
        parseDirStr _ = []

directionsToPositions :: [Dir] -> [Pos]
directionsToPositions = tail . dtp (Pos 0 0)
        where 
            dtp p@(Pos x y) (Up:dirs)    = p : dtp (Pos x (y + 1)) dirs
            dtp p@(Pos x y) (Down:dirs)  = p : dtp (Pos x (y - 1)) dirs
            dtp p@(Pos x y) (Left:dirs)  = p : dtp (Pos (x - 1) y) dirs
            dtp p@(Pos x y) (Right:dirs) = p : dtp (Pos (x + 1) y) dirs
            dtp p [] = [p]

dist :: Pos -> Int
dist (Pos x y) = sum $ map abs [x, y]

part1 :: [[Dir]] -> Int
part1 = S.findMin . S.map dist . foldl1 S.intersection . map (S.fromList . directionsToPositions)

part2 :: [[Dir]] -> Int
part2 xs = minimum allSteps
        where
            positions = map (S.fromList
               . map (\(i, x) -> Indexed i x)
               . (zip [1..])
               . directionsToPositions) xs
            intersections = foldl1 S.intersection positions
            allIntersections = concatMap (S.elems . S.filter (`S.member` intersections)) positions
            allSteps = map (sum . map getIndex) . group . sort $ allIntersections 

split :: String -> String -> [String]
split _ "" = []
split sep str = let (left, right) = splitAt idx str in left : split sep (drop (length sep) right)
        where idx = fromMaybe (length str) $ findIndex (isPrefixOf sep) (tails str)

examples :: [String]
examples = ["U7,R6,D4,L4", "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83", "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]

main :: IO()
main = do
    f <- readFile("3.in")
    let input = map parseWire $ lines f
    -- examples
    -- mapM_ (print . part2 . map parseWire . lines) examples 
    putStrLn "Solution for part 1:"
    print $ part1 input
    putStrLn "Solution for part 2:"
    print $ part2 input

-- Solution part 1: 316
-- Solution part 2: 16368
