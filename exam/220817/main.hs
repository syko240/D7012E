e1 :: [(a -> b) -> [a] -> [b]] -> [(a -> b) -> [a] -> [b]]
e1 = (:) map

f3 x y = x ++ y ++ ["d"]

onezero = 1 : zeroone
zeroone = 0 : onezero

firstfit :: Int -> [Int] -> [[Int]]
firstfit fit (item:items) = process [[item]] fit items

process :: [[Int]] -> Int -> [Int] -> [[Int]]
process bins _ [] = bins
process bins fit (item:items) = 
    process (place bins fit item) fit items

place :: [[Int]] -> Int -> Int -> [[Int]]
place [] _ item = [[item]]
place (bin:bins) fit item
    | volume bin + item <= fit = (item : bin) : bins
    | otherwise = bin : (place bins fit item)

volume :: [Int] -> Int
volume [] = 0
volume (x : xs) = x + volume xs