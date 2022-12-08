#! /usr/bin/env -S runhaskell
import Text.Read (readMaybe)

data File = File Int | Dir [File] deriving (Show, Read)

type Input = [Int]
type Output = Int

parse :: String -> Maybe Input
parse = (snd . getSizes <$>) . readMaybe . removeTrailing . balance 0 . transform . lines
  where
    transform [] = ""
    transform ("$ ls":ls)                  = transform ls
    transform (('d':'i':'r':' ':_):ls)     = transform ls
    transform ("$ cd ..":ls)               = "]," ++ transform ls
    transform (('$':' ':'c':'d':' ':_):ls) = "Dir [" ++ transform ls
    transform (file:ls)                    = let [x, _] = words file; in "File " ++ x ++ ',' : transform ls
    balance l [] = concat $ replicate l "],"
    balance l (x:xs) = x : balance (case x of '[' -> l+1; ']' -> l-1; _ -> l) xs
    removeTrailing (x:y:xs) = if [x,y] == ",]" then y : removeTrailing xs else x : removeTrailing (y:xs)
    removeTrailing (",") = []
    getSizes (File n) = (n, [])
    getSizes (Dir xs) = let (sums, sizes) = unzip $ map getSizes xs in ((sum sums), (sum sums) : (concat sizes))

part1 :: Input -> Output
part1 = sum . filter (<=100000)

part2 :: Input -> Output
part2 sizes = let thres = (maximum sizes) - 40000000 in minimum $ filter (>=thres) sizes

main :: IO()
main = do
    f <- readFile("7.in")
    let input = parse f
    case input of
      Nothing -> putStrLn "Unable to read input."
      Just i -> do
        putStr "Solution for part 1: "
        putStrLn . show $ part1 i
        putStr "Solution for part 2: "
        putStrLn . show $ part2 i

-- Solution for part 1: 1453349
-- Solution for part 2: 2948823
