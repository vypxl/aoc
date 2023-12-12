module Util where

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Text.Read (readMaybe)

import Control.Concurrent.MVar
import System.IO.Unsafe(unsafePerformIO)

type Grid = [[Int]]
type BGrid = [[Bool]]

if' :: Bool -> a -> a -> a
if' c a b = if c then a else b

(?) :: Bool -> a -> a -> a
(?) = if'
infix 1 ?

divides :: Int -> Int -> Bool
divides = ((==0) . ) . flip mod

superlines :: String -> [String]
superlines = splitOn "\n\n"

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

capitalize :: String -> String
capitalize = (:) <$> toUpper . head <*> tail

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

-- Taken from https://hackage.haskell.org/package/uglymemo
-- By Lennart Augustsson
memoIO :: (Ord a) => (a -> b) -> IO (a -> IO b)
memoIO f = do
    v <- newMVar M.empty
    let f' x = do
            m <- readMVar v
            case M.lookup x m of
                Nothing -> do let { r = f x }; modifyMVar_ v (return . M.insert x r); return r
                Just r  -> return r
    return f'

memo :: (Ord a) => (a -> b) -> (a -> b)
memo f = let f' = unsafePerformIO (memoIO f) in \x -> unsafePerformIO (f' x)
