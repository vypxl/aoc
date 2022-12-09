module Util where

import Data.Char
import Data.List
import Data.List.Split
import Text.Read (readMaybe)

type Grid = [[Int]]
type BGrid = [[Bool]]

superlines :: String -> [String]
superlines = splitOn "\n\n"

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

grid :: String -> Maybe [[Int]]
grid = mapM (mapM (readMaybe . return)) . lines

printGrid :: Grid -> IO ()
printGrid = mapM_ print

reducerGrid :: (Int -> Int -> Int) -> Grid -> Int
reducerGrid f = foldr1 f . map (foldr1 f)

printBGrid :: BGrid -> IO ()
printBGrid = mapM_ ((putStrLn . concat . map show) . map fromEnum)

sumBGrid :: BGrid -> Int
sumBGrid = sum . map (sum . map fromEnum)

tuple2 :: [a] -> Maybe (a, a)
tuple2 [x, y] = Just (x, y)
tuple2 _ = Nothing

tuple3 :: [a] -> Maybe (a, a, a)
tuple3 [x, y, z] = Just (x, y, z)
tuple3 _ = Nothing

issubset :: Eq a => [a] -> [a] -> Bool
issubset = (null . ) . (\\)

count :: (a -> Bool) -> [a] -> Int
count = (length . ) . filter

applyN :: (a -> a) -> Int -> a -> a
applyN f i x = (iterate f x) !! i

dropLast :: Int -> [a] -> [a]
dropLast = applyN init

(#) :: a -> a -> [a]
(#) = ( . return ) . (:)

