-- Author: André Roaas

import Data.Char (ord,chr)

-- Sorting

merge :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
merge left [] = left
merge [] right = right
merge ((l1, l2, l3) : left) ((r1, r2, r3) : right)
    | l1 <= r1 = (l1, l2, l3) : merge left ((r1, r2, r3) : right)
    | otherwise = (r1, r2, r3) : merge ((l1, l2, l3) : left) right

sortList :: [(Int, Int, Int)] -> [(Int, Int, Int)]
sortList [] = []
sortList [x] = [x]
sortList lst = let (left, right) = splitAt (length lst `div` 2) lst
               in merge (sortList left) (sortList right)

-- Compute Sublists

findSets :: [Int] -> [(Int, Int, Int)]
findSets lst = [(sum (subList i j), i, j) | i <- [0..length lst - 1], j <- [i..length lst - 1]]
    where
        subList start end = take (end - start + 1) (drop start lst)

smallestKSets :: Int -> [Int] -> [(Int, Int, Int)]
smallestKSets _ [] = error "list is empty"
smallestKSets k lst = take k (sortList (findSets lst))

-- Output Formating

output :: [Int] -> [(Int, Int, Int)] -> IO ()
output lst res = do
    putStrLn ("\tEntire list: " ++ show lst ++ "\n")
    putStrLn "\tsize\ti\tj\tsublist"
    printRows res
  where
    printRows [] = return ()
    printRows ((sum, i, j) : remainingRows) = do
        let sublistStr = show (extractSublist i j lst)
        putStrLn ("\t" ++ show sum ++ "\t" ++ show (i + 1) ++ "\t" ++ show (j + 1) ++ "\t" ++ sublistStr)
        printRows remainingRows

    extractSublist start end lst = take (end - start + 1) (drop start lst)

-- Testing

testCases :: [(Int, [Int])]
testCases = [
    (15, [x*(-1)^x | x <- [1..100]]),
    (6, [24, -11, -34, 42, -24, 7, -19, 21]),
    (8, [3, 2, -4, 3, 2, -5, -2, 2, 3, -3, 2, -5, 6, -2, 2, 3])
    ]

test :: [(Int, [Int])] -> IO ()
test [] = return ()
test ((k, lst) : remainingTests) = do
    let res = smallestKSets k lst
    output lst res
    putStrLn ""
    test remainingTests

main :: IO ()
main = test testCases