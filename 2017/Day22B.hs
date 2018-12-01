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
    let res = p2 10000000 parsed
    print $ last res
    -- print $ res
    return ()

type Vec = (Int, Int)

addVec :: Vec -> Vec -> Vec
addVec (x1, y1) (x2, y2) = (x1+x2, y1+y2)

data Dir = N | O | S | W deriving (Eq, Show)

dirToVec :: Dir -> Vec
dirToVec N = ( 0,-1)
dirToVec S = ( 0, 1)
dirToVec O = ( 1, 0)
dirToVec W = (-1, 0)

reverseDir :: Dir -> Dir
reverseDir N = S
reverseDir S = N
reverseDir O = W
reverseDir W = O

data TurnDir = Left | Right deriving (Eq, Show)

turn :: Dir -> TurnDir -> Dir
turn N Left  = W
turn N Right = O
turn O Left  = N
turn O Right = S
turn S Left  = O
turn S Right = W
turn W Left  = S
turn W Right = N

data State = Clean | Weakened | Infected | Flagged deriving (Eq, Show)

next :: State -> State
next Clean = Weakened
next Weakened = Infected
next Infected = Flagged
next Flagged = Clean

type Grid = M.Map Vec State

at :: Vec -> Grid -> (State, Grid)
at xy grid = process lookedUp
    where lookedUp = M.lookup xy grid
          process Nothing = (Clean, M.insert xy Clean grid)
          process (Just it) = (it, grid)

--    -y
--    |
-- -x-+-+x
--    |
--    +y

parse :: String -> Grid
parse input = M.fromList [((x-mid, y-mid), if split!!y!!x == '#' then Infected else Clean) | x <- [0..len-1], y <- [0..len-1]]
    where split = splitOn "\n" input
          len = length split - 1
          mid = div len 2

burst :: (Vec, Dir, Grid, Int) -> (Vec, Dir, Grid, Int)
burst (xy, dir, grid, sum) = (nXY, nDir, nGrid, nSum)
    where a = at xy grid
          decDir dir Clean    = turn dir Left
          decDir dir Weakened = dir
          decDir dir Infected = turn dir Right
          decDir dir Flagged  = reverseDir dir
          nDir = decDir dir $ fst a
          nGrid = M.insert xy (next $ fst a) $ snd a
          nSum = if fst a == Weakened then sum + 1 else sum
          nXY = addVec xy $ dirToVec nDir

p2 :: Int -> Grid -> [Int]
p2 iters grid = tail . map fourth . take (iters + 1) $ iterate burst ((0, 0), N, grid, 0)
    where fourth (_, _, _, it) = it
