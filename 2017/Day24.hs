module Day24 where

import Data.List.Split (splitOn)
import Data.List (maximumBy)
import Debug.Trace

main :: IO()
main = do
    --let input = "0/2\n2/2\n2/3\n3/4\n3/5\n0/1\n10/1\n9/10"
    --2/2;20/20;12/12;5/5;18/18;4/4;49/49;29/29;
    let input = "32/31\n0/43\n45/15\n33/24\n14/42\n2/35\n50/27\n2/17\n5/45\n3/14\n26/1\n33/38\n29/6\n50/32\n9/48\n36/34\n33/50\n37/35\n26/13\n19/4\n14/46\n17/29\n45/43\n5/0\n41/22\n50/3\n17/1\n40/7\n19/0\n33/7\n22/48\n9/14\n50/43\n26/29\n19/33\n46/31\n3/16\n29/46\n16/0\n34/17\n31/7\n5/27\n7/4\n14/21\n50/9\n14/44\n13/38\n31/11"
    --input <- readFile "input24.txt"
    let parsed = concatMap (\c@(Comp l r) -> [c, Comp r l]) $ parse input
    print parsed
    --print $ strength parsed
    --print $ valid parsed
    --mapM_ print $ bridges parsed [Comp 0 0]
    let two = bridges2 parsed [Comp 0 0]
    let longests = [x | x <- two, length x > 27]
    mapM_ (print . length) longests
    mapM_ print longests
    --let best = foldl1 (\acc new -> if length new > length acc || (length new == length acc && strength new > strength acc) then new else acc) two
    let best = foldl1 (\acc new -> if length new > length acc || (length new == length acc && strength new > strength acc) then new else acc) two
    putStrLn "-----"
    print best
    --print $ maximumBy (\a b -> compare (length a) $ length b)
    print $ strength best
    --print $ p1 parsed
    --print . strength $ p1 parsed

data Comp = Comp Int Int deriving (Show, Read)

instance Eq Comp where
    (==) (Comp a b) (Comp x y) = (a == x && b == y) || (a == y && b == x)

compFromStr :: String -> Comp
compFromStr str = Comp (read (head split) :: Int) (read (last split) :: Int)
    where split = splitOn "/" str

cStrength :: Comp -> Int
cStrength (Comp a b) = a + b

connectionValid :: Comp -> Comp -> Bool
connectionValid (Comp _ a) (Comp b _) = a == b

type Bridge = [Comp]

strength :: Bridge -> Int
strength bridge = sum $ map cStrength bridge

--valid :: Bridge -> Bool
--valid = fst . foldl checkConnection (True, Comp 0 0)
--    where checkConnection (validUntilHere, Comp _ left) newComp@(Comp right _) = (validUntilHere && left == right, newComp)


parse :: String -> [Comp] -- NOT Bridge
parse str = map compFromStr $ lines str

--p1 :: [Comp] -> Bridge
--p1 comps = maximumBy (\l r -> compare (strength l) $ strength r) $ bridges comps [Comp 0 0]

--bridges :: [Comp] -> Bridge -> [Bridge]
--bridges comps seen = if null possible then [seen] else seen : concatMap (bridges comps) new where
--        end = last seen
--        possible = filter (`notElem` seen) comps
--        new = [seen ++ [el] | el <- possible, connectionValid end el]

bridges :: [Comp] -> Bridge -> Bridge
bridges comps seen = if null newComps then trace (show seen) seen else maximumBy (\l r -> compare (strength l) $ strength r) [bridges (newComps!!i) $ new!!i | i <- [0..length new - 1]] where
        end = last seen
        --possible = filter (`notElem` seen) comps
        possible = filter (connectionValid end) comps
        new = [seen ++ [el] | el <- possible]
        newComps = [filter (`notElem` it) comps | it <- new]

bridges2 :: [Comp] -> Bridge -> [Bridge]
bridges2 comps seen = if null newComps then [seen] else concat [bridges2 (newComps!!i) $ new!!i | i <- [0..length new - 1]] where
        end = last seen
        --possible = filter (`notElem` seen) comps
        possible = filter (connectionValid end) comps
        new = [seen ++ [el] | el <- possible]
        newComps = [filter (`notElem` it) comps | it <- new]

