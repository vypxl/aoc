module Util where

import Data.Char
import Data.List
import Data.List.Split

superlines :: String -> [String]
superlines = splitOn "\n\n"

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

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

