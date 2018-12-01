import           Prelude hiding (Left, Right)
import           Data.List
import           Data.List.Split
import qualified Data.Map as M

main :: IO()
main = do
    --let input = "..#\n#..\n...\n"
    input <- readFile "input22.txt"
    let parsed = parse input
    print parsed
    print $ p1 7 parsed
    print $ p1 70 parsed
    print . last $ p1 10000 parsed
    return ()

type Vec = (Int, Int)

addVec :: Vec -> Vec -> Vec
addVec (x1, y1) (x2, y2) = (x1+x2, y1+y2)

data Dir = N | O | S | W deriving (Show)

dirToVec :: Dir -> Vec
dirToVec N = ( 0,-1)
dirToVec S = ( 0, 1)
dirToVec O = ( 1, 0)
dirToVec W = (-1, 0)

data TurnDir = Left | Right deriving (Show)

turn :: Dir -> TurnDir -> Dir
turn N Left  = W
turn N Right = O
turn O Left  = N
turn O Right = S
turn S Left  = O
turn S Right = W
turn W Left  = S
turn W Right = N

type Grid = M.Map Vec Bool

at :: Vec -> Grid -> (Bool, Grid)
at xy grid = process lookedUp
    where lookedUp = M.lookup xy grid
          process Nothing = (False, M.insert xy False grid)
          process (Just it) = (it, grid)

--    -y
--    |
-- -x-+-+x
--    |
--    +y

parse :: String -> Grid
parse input = M.fromList [((x-mid, y-mid), split!!y!!x == '#') | x <- [0..len-1], y <- [0..len-1]]
    where split = splitOn "\n" input
          len = length split - 1
          mid = div len 2

burst1 :: (Vec, Dir, Grid, Int) -> (Vec, Dir, Grid, Int)
burst1 (xy, dir, grid, sum) = (nXY, nDir, nGrid, nSum)
    where a = at xy grid
          nDir = if fst a then turn dir Right else turn dir Left
          nGrid = M.insert xy (not $ fst a) $ snd a
          nSum = if fst a then sum else sum + 1
          nXY = addVec xy $ dirToVec nDir

p1 :: Int -> Grid -> [Int]
p1 iters grid = tail . map fourth . take (iters + 1) $ iterate burst1 ((0, 0), N, grid, 0)
    where fourth (_, _, _, it) = it
