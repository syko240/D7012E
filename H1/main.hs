import Data.Char (ord,chr)

sumList :: Num a => [a] -> a
sumList list = foldr (+) 0 list

merge :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
merge xs [] = xs
merge [] ys = ys
merge ((x1, x2, x3) : xs) ((y1, y2, y3) : ys)
    | x1 <= y1  = (x1, x2, x3) : merge xs ((y1, y2, y3) : ys)
    | otherwise = (y1, y2, y3) : merge ((x1, x2, x3) : xs) ys

sortList :: [(Int, Int, Int)] -> [(Int, Int, Int)]
sortList []  = []
sortList [x] = [x]
sortList xs  = let (firstHalf, secondHalf) = splitAt (length xs `div` 2) xs
               in merge (sortList firstHalf) (sortList secondHalf)

findSets :: [Int] -> [(Int, Int, Int)]
findSets list = [(sumList (subList i j), i, j) | i <- [0..length list - 1], j <- [i..length list - 1]]
    where
        subList start end = take (end - start + 1) (drop start list)

smallestKSets :: Int -> [Int] -> [(Int, Int, Int)]
smallestKSets _ [] = error "list is empty"
smallestKSets k list = take k (sortList (findSets list))

output :: Int -> [Int] -> [(Int, Int, Int)] -> IO ()
output k list res = do
    putStrLn "\tsize\ti\tj\tsublist"
    printTuples (take k res)
  where
    printTuples [] = return ()
    printTuples ((a, b, c) : xs) = do
        let sublistStr = show (subList b c list)
        putStrLn ("\t" ++ show a ++ "\t" ++ show (b+1) ++ "\t" ++ show (c+1) ++ "\t" ++ sublistStr)
        printTuples xs

    subList i j lst = take (j - i + 1) $ drop i lst

main :: IO ()
main = do
    let k = 3 :: Int
    let list = [-1, 2, -3, 4, -5] :: [Int]
    let res = smallestKSets k list
    output k list res
    --print ( smallestKSets k list )