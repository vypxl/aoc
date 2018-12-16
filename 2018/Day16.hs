#! /usr/bin/env -S stack runhaskell
import Data.List
import Data.Bits

data State = State Int Int Int Int deriving (Show, Eq)
data Opcode = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr deriving (Show, Eq)
data Op = Op Int Int Int Int deriving (Show, Eq)
data Sample = Sample State Op State deriving (Show, Eq)

opCodeList :: [Opcode]
opCodeList = [Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori, Setr, Seti, Gtir, Gtri, Gtrr, Eqir, Eqri, Eqrr]

opList :: [State -> Op -> State]
opList = pure operate <*> opCodeList

split :: String -> String -> String -> [String]
split str sep acc
    | str == [] = [reverse acc]
    | take (length sep) str == sep = (reverse acc) : (split (drop (length sep) str) sep "")
    | otherwise = let (x:xs) = str in split xs sep (x : acc)

readState :: String -> State
readState str =
    let [a, b, c, d] = map (\x -> read x :: Int) (split ((drop 9 . takeWhile (/= ']')) str) ", " "")
    in State a b c d

readOp :: String -> Op
readOp str = 
    let [op, a, b, c] = map(\x -> read x :: Int) (words str)
    in Op op a b c

getReg :: State -> Int -> Int
getReg (State a _ _ _) 0 = a
getReg (State _ b _ _) 1 = b
getReg (State _ _ c _) 2 = c
getReg (State _ _ _ d) 3 = d

setReg :: State -> Int -> Int -> State
setReg (State _ b c d) 0 v = State v b c d
setReg (State a _ c d) 1 v = State a v c d
setReg (State a b _ d) 2 v = State a b v d
setReg (State a b c _) 3 v = State a b c v

operate :: Opcode -> State -> Op -> State
operate Addr state (Op _ a b c) = setReg state c $ (getReg state a)  + (getReg state b)
operate Addi state (Op _ a b c) = setReg state c $ (getReg state a)  + b
operate Mulr state (Op _ a b c) = setReg state c $ (getReg state a)  * (getReg state b)
operate Muli state (Op _ a b c) = setReg state c $ (getReg state a)  * b
operate Banr state (Op _ a b c) = setReg state c $ (getReg state a) .&. (getReg state b)
operate Bani state (Op _ a b c) = setReg state c $ (getReg state a) .&. b
operate Borr state (Op _ a b c) = setReg state c $ (getReg state a) .|. (getReg state b)
operate Bori state (Op _ a b c) = setReg state c $ (getReg state a) .|. b
operate Setr state (Op _ a _ c) = setReg state c $ (getReg state a)
operate Seti state (Op _ a _ c) = setReg state c $ a
operate Gtir state (Op _ a b c) = setReg state c $ if a > (getReg state b) then 1 else 0
operate Gtri state (Op _ a b c) = setReg state c $ if (getReg state a) > b then 1 else 0
operate Gtrr state (Op _ a b c) = setReg state c $ if (getReg state a) > (getReg state b) then 1 else 0
operate Eqir state (Op _ a b c) = setReg state c $ if a == (getReg state b) then 1 else 0
operate Eqri state (Op _ a b c) = setReg state c $ if (getReg state a) == b then 1 else 0
operate Eqrr state (Op _ a b c) = setReg state c $ if (getReg state a) == (getReg state b) then 1 else 0

filterOutAmbiguities :: [[Opcode]] -> [Opcode]
filterOutAmbiguities ops
    | all ((==1) . length) ops = map head ops
    | otherwise =
        let determined = map head . filter ((==1) . length) $ ops
        in filterOutAmbiguities . map (\xs -> if length xs == 1 then xs else xs \\ determined) $ ops

analyze :: [Sample] -> [[Opcode]] -> [State -> Op -> State]
analyze [] ops = map operate . filterOutAmbiguities $ ops
analyze ((Sample before op@(Op opcode _ _ _) after):xs) ops = analyze xs newops
    where
        current = ops!!opcode
        filtered = filter (\x -> operate x before op == after) current
        newops = (take opcode ops) ++ filtered : (drop (opcode + 1) ops)

execute :: [State -> Op -> State] -> [Op] -> State -> State
execute _ [] state = state
execute ops (op@(Op opcode _ _ _):xs) state = execute ops xs $ (ops!!opcode) state op

part1 :: [Sample] -> Int
part1 = length . filter (\(Sample before op after) -> (length . filter (==after) $ opList <*> pure before <*> pure op) >= 3)

part2 :: [Sample] -> [Op] -> Int
part2 samples program = let (State a _ _ _) = execute (analyze samples (replicate 16 opCodeList)) program (State 0 0 0 0) in a

main :: IO()
main = do
    f <- readFile("16.in")
    let [inp1, inp2] = split f "\n\n\n\n" ""
    let input1 = [ Sample (readState l1) (readOp l2) (readState l3) | sp <- split inp1 "\n\n" "", let [l1, l2, l3] = lines sp]
    let input2 = [ readOp x | x <- lines inp2]
    putStrLn "Solution for part 1:"
    print $ part1 input1
    putStrLn "Solution for part 2:"
    print $ part2 input1 input2

-- Solution part 1: 612
-- Solution part 2: 485
