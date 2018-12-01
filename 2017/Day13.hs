import Data.List
import Data.List.Split
import qualified Data.Map as M
import System.IO.Unsafe

import Debug.Trace

readNum :: String -> Int
readNum s = read s :: Int

da = map (\s -> (read ((splitOn ": " s)!!0) :: Int, (read $ (splitOn ": " s)!!1) :: Int)) $ splitOn "\n" $ unsafePerformIO $ readFile "input13.txt"
-- da = map (\s -> (readNum ((splitOn ": " s)!!0), readNum ((splitOn ": " s)!!1))) $ splitOn "\n" "0: 3\n1: 2\n4: 4\n6: 4"

m = foldl (\acc (k, v) -> M.insert k v acc) fresh $ da
    where fresh = M.fromList $ map (\i -> (i, 0)) [0..(maximum $ map (fst) da)]

l = M.toList m

ks = M.keys m
els = M.elems m

poses = map (\el
    -> map (\i -> if el /= 0 then i `mod` (el * 2 - 2) else -1) [0..(length m + 100001)]
    ) $ els

main :: IO()
main = do
    -- print m
    -- print poses
    print $ save 0
    print $ recSave 0

p1 :: M.Map Int Int -> Int
p1 m = sum $ map (\el -> el * (els!!el) * ifCaught el) $ ks
    where
        caught i = (poses!!i)!!i == 0
        ifCaught i = if caught i then 1 else 0

save :: Int -> Bool
save delay = not . or $ map (caught) $ ks
    where caught i = (poses!!i)!!(i + delay) == 0

recSave :: Int -> Int
recSave delay
    | save delay = delay
    | otherwise = recSave $ delay + 1

recSave' :: Int -> Int
recSave' delay
    | save delay = delay
    | otherwise = trace ("delay = " ++ show delay) $ recSave' $ delay + 1

recSave'' :: Int
recSave'' = head [i | i <- [0..], not $ caught i]
    where caught i = or [(i+d) `mod` (r * 2 - 2) == 0 | (d,r) <- M.toList m]

recSave''' :: Int
recSave''' = head [i | i <- [0..], not $ caught i]
    where caught i = or [(i+d) `mod` (r * 2 - 2) == 0 | (d,r) <- da]

