import Data.List
import Data.List.Split
import System.IO.Unsafe

da = map (\s -> (read ((splitOn " <-> " s)!!0) :: Int, read ('[' : ((splitOn " <-> " s)!!1) ++ "]"))) $ splitOn "\n" $ unsafePerformIO $ readFile "input12.txt"

main :: IO()
main = do
    print $ length $ recZeros [0] da
    print $ groups da

recZeros :: (Eq a, Num a) => [a] -> [(a, [a])] -> [a]
recZeros seen d
  | length new > length seen = recZeros new d
  | otherwise = seen
  where new = zeros seen d

zeros :: (Eq a, Num a) => [a] -> [(a, [a])] -> [a]
zeros start d = foldl (\acc (id, conns) 
  -> if not . null $ intersect acc conns then 
    (if elem id acc then acc else id : acc) 
  else 
    acc) start $ tail d

groups :: (Eq a, Num a) => [(a, [a])] -> a
groups [] = 0
groups remaining@(x:xs) = (+) 1 $ groups $ filter ((\el -> not $ elem (fst el) rz)) remaining
    where rz = recZeros [fst x] remaining