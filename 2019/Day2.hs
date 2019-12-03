#! /usr/bin/env -S stack runhaskell
import Data.List
import Data.Maybe

run :: [Integer] -> Integer -> Integer -> Integer
run mem noun verb = head $ operate 0 ([head mem, noun, verb] ++ (drop 3 mem))

operate :: Int -> [Integer] -> [Integer]
operate ip mem
    | op == 1 = operate (ip + 4) $ wr c $ (rd a) + (rd b)
    | op == 2 = operate (ip + 4) $ wr c $ (rd a) * (rd b)
    | op == 99 = mem
    | otherwise = [-1]
    where
        (op:a:b:c:_) = drop ip mem
        wr pos val = let (xs,_:ys) = splitAt (fromIntegral pos) mem in xs ++ val : ys
        rd pos = mem!!(fromIntegral pos)

part1 :: [Integer] -> Integer
part1 input = run input 12 2

part2 :: [Integer] -> Integer
part2 input = snd . head $ dropWhile ((/=19690720) . fst) [(run input n v, 100 * n + v) | v <- [0..99], n <- [0..99]]

split :: String -> String -> [String]
split _ "" = []
split sep str = let (left, right) = splitAt (idx) str in left : split sep (drop (length sep) right)
        where idx = fromMaybe (length str) $ findIndex (isPrefixOf sep) (tails str)

parse :: String -> [Integer]
parse str = map read $ split "," str

examples :: [[Integer]]
examples = map parse ["1,9,10,3,2,3,11,0,99,30,40,50", "1,0,0,0,99", "2,3,0,3,99", "2,4,4,5,99,0", "1,1,1,4,99,5,6,0,99"]

main :: IO()
main = do
    f <- readFile("2.in")
    let input = parse f
    -- Examples
    -- mapM_ print $ map (operate 0) examples
    putStrLn "Solution for part 1:"
    print $ part1 input
    putStrLn "Solution for part 2:"
    print $ part2 input

-- Solution part 1: 3166704
-- Solution part 2: 8018
