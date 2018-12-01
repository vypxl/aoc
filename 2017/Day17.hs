import Data.List

input = 303

main :: IO()
main = do
    -- print $ one
    -- print $ elemIndex 2017 one
    print $ (elemIndex 2017 one) >>= \i -> return $ one!!(i+1)
    print $ (show $ one !! 1 ) ++ " " ++ (show $ two)
    where one = p1 input 2017
          two = p2 input 50000000


p1 :: Int -> Int -> [Int]
p1 step iters = foldl' (\acc cur ->
        let i = ((unjust $ elemIndex (cur - 1) acc) + step) `mod` length acc
        in (take (i+1) acc) ++ (cur : (drop (i+1) acc))
        ) [0] [1..iters]
    where unjust (Just a) = a

p2 :: Int -> Int -> Int
p2 step iters = snd $ foldl' (\acc cur ->
        let i = ((fst acc) + step) `mod` cur
        in if i == 0 then (i+1, cur) else (i+1, snd acc)
        ) (0, 0) [1..iters]
    where unjust (Just a) = a