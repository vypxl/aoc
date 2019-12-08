#! /usr/bin/env -S stack runhaskell
import Data.List
import Data.Char (digitToInt, isNumber)
import Data.Ord (comparing)
import Data.List.Split (chunksOf)

data Image = Image Int Int [[[Int]]] deriving (Show)

collapseImage :: Image -> Image
collapseImage (Image w h _layers) = Image w h [chunksOf w . collapse' . map concat $ _layers]
    where
        collapse' layers
            | concat layers == [] = []
            | otherwise = (head . dropWhile (==2) . map head $ layers) : collapse' (map tail layers)

imageData :: Image -> [[[Int]]]
imageData (Image _ _ layers) = layers

renderImage :: Image -> String
renderImage img@(Image w _ _) = ('▒' : (replicate w '▒') ++ "▒\n") ++ simg ++ ('▒' : (replicate w '▒') ++ "▒")
        where
            surround = (\l -> '▒' : l ++ "▒\n")
            getSymbol '0' = ' '
            getSymbol '1' = '█'
            getSymbol x = x
            simg = map (getSymbol) . concatMap (surround . (concatMap show)) . head . imageData . collapseImage $ img

part1 :: Image -> Int
part1 (Image _ _ layers) = (count 1) * (count 2)
    where 
        chosen = minimumBy (comparing (length . filter (==0))) $ map concat layers
        count val = length $ filter (==val) chosen

part2 :: Image -> String
part2 = renderImage

parse :: Int -> Int -> String -> Image
parse w h str = Image w h (parse' (filter (isNumber) str))
    where
        parse' [] = []
        parse' s = parseLayer (take (w * h) s) : parse' (drop (w * h) s)
        parseLayer layer = [[digitToInt c | c <- row] | row <- chunksOf w layer]

main :: IO()
main = do
    let w = 25
    let h = 6
    f <- readFile("8.in")
    let input = parse w h f
    putStrLn "Solution for part 1:"
    print $ part1 input
    putStrLn "Solution for part 2:"
    putStrLn $ part2 input

-- Solution part 1: 1792
-- Solution part 2: ljech
