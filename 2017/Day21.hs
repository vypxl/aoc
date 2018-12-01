import Data.List
import Data.List.Split

main :: IO()
main = do
    input <- readFile "input21.txt"
    -- let input = "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"
    let parsed = parse input
    let begin = [[0,1,0], [0,0,1], [1,1,1]]
    print parsed
    let x = p1 begin parsed 18
    mapM_ print $ zip [0..] (map count x)
    --print . count . last $ p1 begin parsed 18
    return ()

display :: Pattern -> IO()
display = putStrLn . concatMap (\x -> show x ++ "\n")

display' :: Pattern -> IO()
display' = putStrLn . concatMap (\x -> show (change x) ++ "\n")
    where change = map (\n -> if n == 0 then '.' else '#')

type Pattern = [[Int]]

size :: Pattern -> Int
size = length

count :: Pattern -> Int
count = length . filter (==1) . concat

rotate :: Pattern -> Int -> Pattern
rotate p 0 = p
rotate p 1 = map reverse $ transpose p
rotate p 2 = rotate (rotate p 1) 1
rotate p 3 = transpose $ map reverse p

pFlip :: Pattern -> Int -> Pattern
pFlip p 0 = p
--pFlip p 1 = map reverse p
pFlip p 1 = reverse p

divide :: Pattern -> Int -> [[Pattern]]
divide p s = [[map (take s . drop x) (take s $ drop y p) | y <- [0,s..size p - 1]] | x <- [0,s..size p - 1]]

combine :: [[Pattern]] -> Pattern
combine pss = concat [[concatMap (!!y) ps | y <- [0..size (head ps) - 1]] | ps <- pss]-- concat [concat ps | ps <- pss]

type Rule = (Pattern, Pattern)

match :: Pattern -> [Rule] -> Rule
match p rules = head $ filter (\rule -> fst rule `elem` flips) rules
    where rots = map (rotate p) [0..3]
          flips = [pFlip r x | r <- rots, x <- [0..1]]

convert :: Pattern -> [Rule] -> Pattern
convert p rules = snd $ match p rules

parse :: String -> [Rule]
parse str = map parseln $ lines str
    where parseln ln = let split = splitOn " => " ln in (convStr (head split), convStr (last split))
          convStr str = [[if c == '.' then 0 else 1 | c <- frac] | frac <- splitOn "/" str]

process1 :: Pattern -> [Rule] -> Pattern
process1 p rules
    | size p `mod` 2 == 0 = combine [map (`convert` rules) xs | xs <- divide p 2]
    | size p `mod` 3 == 0 = combine [map (`convert` rules) xs | xs <- divide p 3]
    | otherwise = error "Not divisible by 2 or 3!"

p1 :: Pattern -> [Rule] -> Int -> [Pattern]
p1 start rules iters = scanl (\p _ -> process1 p rules) start [1..iters]
