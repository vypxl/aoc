import Data.List
import Data.List.Split
import Control.Arrow
import Debug.Trace

main :: IO()
main = do
    input <- readFile "input19.txt"
    let eqLines = map (\l -> l ++ replicate (len - length l) ' ') ls
                    where ls = lines input
                          len = maximum $ map length ls
    putStrLn $ p1 eqLines 'N' (0, 13) ""
    print $ p2 eqLines 'N' (0, 13) 0
    return ()


type Pos = (Int, Int)

addV :: Pos -> Pos -> Pos
addV one = (+) (fst one) *** (+) (snd one)

opposite :: Char -> Char
opposite 'N' = 'S'
opposite 'S' = 'N'
opposite 'O' = 'W'
opposite 'W' = 'O'

inGrid :: [String] -> Pos -> Bool
inGrid grid pos = fst pos >= 0 && fst pos < length grid && snd pos >= 0 && snd pos < length (head grid)

at :: [String] -> Pos -> Char
at grid pos = (grid !! fst pos) !! snd pos

p1 :: [String] -> Char -> Pos -> String -> String
p1 grid dir pos seen
    | null neighbours = seen
    | length neighbours == 1 = run $ head neighbours
    | otherwise = run . unjust $ find (\n -> fst n == dir) neighbours
    where
        neighbours = filter (\v -> inGrid grid (snd v) && fst v /= opposite dir && at grid (snd v) /= ' ') $ map (\t -> (fst t, addV pos (snd t))) [('O', (0,1)), ('W', (0,-1)), ('N', (1,0)), ('S', (-1,0))]
        run neighbour = uncurry (p1 grid) neighbour $
                            if at grid (snd neighbour) `elem` ['A'..'Z'] then seen ++ [at grid (snd neighbour)] else seen
        unjust (Just x) = x

p2 :: [String] -> Char -> Pos -> Int -> Int
p2 grid dir pos steps
    | null neighbours = steps + 1
    | length neighbours == 1 = run $ head neighbours
    | otherwise = run . unjust $ find (\n -> fst n == dir) neighbours
    where
        neighbours = filter (\v -> inGrid grid (snd v) && fst v /= opposite dir && at grid (snd v) /= ' ') $ map (\t -> (fst t, addV pos (snd t))) [('O', (0,1)), ('W', (0,-1)), ('N', (1,0)), ('S', (-1,0))]
        run neighbour = uncurry (p2 grid) neighbour $ steps + 1
        unjust (Just x) = x