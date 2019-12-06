#! /usr/bin/env -S stack runhaskell
import Data.List
import Data.Maybe
import qualified Data.Set as S

data Orbit = Orbit String [Orbit]

instance Show Orbit where
    show = show' 1
        where show' d (Orbit name xs) = (intersperse '│' $ replicate (d) ' ') ++ "┝╸" ++ name ++ "\n" ++ (intercalate "" (map (show' (d + 1)) xs))

createOrbit :: [(String, String)] -> Orbit
createOrbit relations = createOrbit' (Orbit root []) relations
    where
        (parents, children) = unzip relations
        root = head . S.elems $ (S.fromList parents) `S.difference` (S.fromList children) -- root = node without parent
        createOrbit' (Orbit name _) rels = Orbit name cs
            where
                childRelations = filter (\(p, _) -> p == name) rels -- childRelations = all relations with current node as parent
                cs = map (\(_, c) -> createOrbit' (Orbit c []) rels) childRelations -- cs = childRelations mapped to their full orbit

countOrbit :: Orbit -> Int
countOrbit = countOrbit' 0
    where countOrbit' depth (Orbit _ xs) = depth + (sum $ map (countOrbit' (depth + 1)) xs)

inOrbit :: String -> Orbit -> Bool
inOrbit name (Orbit oName xs) = name == oName || any (inOrbit name) xs

distanceTo :: String -> Orbit -> Int
distanceTo = distanceTo' 0
    where
        distanceTo' depth dest orb@(Orbit _ xs)
            | not $ inOrbit dest orb = 0
            | any (\(Orbit n _) -> n == dest) xs = depth + 1
            | otherwise = sum $ map (distanceTo' (depth + 1) dest) xs

distanceBetween :: String -> String -> Orbit -> Int
distanceBetween src dst orb@(Orbit _ xs)
    | not $ inOrbit src orb || inOrbit dst orb = 0
    | inOrbit src orb && (not $ inOrbit dst orb) = distanceTo src orb
    | inOrbit dst orb && (not $ inOrbit src orb) = distanceTo dst orb
    | otherwise = sum $ map (distanceBetween src dst) xs

part1 :: Orbit -> Int
part1 = countOrbit

part2 :: Orbit -> Int
part2 = distanceBetween "YOU" "SAN"

parse :: String -> Orbit
parse = createOrbit . map (\l -> let (p:c:_) = split ")" l in (p, c)) . lines

split :: String -> String -> [String]
split _ "" = []
split sep str = let (left, right) = splitAt idx str in left : split sep (drop (length sep) right)
        where idx = fromMaybe (length str) $ findIndex (isPrefixOf sep) (tails str)

example :: String
example = "G)H\nB)C\nC)D\nD)E\nE)F\nCOM)B\nB)G\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"

main :: IO()
main = do
    f <- readFile("6.in")
    let input = parse f
    -- let input = parse example
    -- print input
    putStrLn "Solution for part 1:"
    print $ part1 input
    putStrLn "Solution for part 2:"
    print $ part2 input

-- Solution part 1: 314702
-- Solution part 2: 439
