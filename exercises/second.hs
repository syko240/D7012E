import Prelude hiding (elem, product, and, or, unzip, reverse, drop, splitAt)
import Data.Char (isAlpha, toLower)

-- 5.2
orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (x, y, z) = (min', mid, max')
  where
    min' = min x (min y z)
    max' = max x (max y z)
    mid = x + y + z - min' - max'

-- 5.10
divisors :: Int -> [Int]
divisors n
    | n > 0 = [x | x <- [1..n], n `mod` x == 0]
    | otherwise = []

isPrime :: Int -> Bool
isPrime n
    | n > 1 = length (divisors n) == 2
    | otherwise = False

-- 5.11
matches :: Int -> [Int] -> [Int]
matches n xs = [x | x <- xs, x == n]

elem :: Int -> [Int] -> Bool
elem n xs = not (null (matches n xs))

-- 5.18
-- The 'shift' function is polymorphic
-- Meaning, the most general type of 'shift' is "shift :: ((a, b), c) -> (a, (b, c))"

-- 5.22
onSeparateLines :: [String] -> String
onSeparateLines [] = ""
onSeparateLines (x : xs) = x ++ "\n" ++ onSeparateLines xs

-- 5.23
duplicate :: String -> Int -> String
duplicate str n
    | n <= 0 = ""
    | n == 1 = str
    | otherwise = str ++ duplicate str (n-1)

duplicate' :: String -> Int -> String
duplicate' str n = concat (replicate n str)

-- 5.24
pushRight :: Int -> String -> String
pushRight linelength str = replicate (linelength - length str) ' ' ++ str

-- 6.29
-- barcode, name, price
type BillType = [(Int, String, Double)]

discountItemCount :: BillType -> Int
discountItemCount [] = 0
discountItemCount ((x, _, _) : xs)
    | x == 1234 = 1 + discountItemCount xs
    | otherwise = discountItemCount xs

makeDiscount :: Int -> Int
makeDiscount x = x `div` 2

calculateTotal :: BillType -> Double
calculateTotal [] = 0
calculateTotal ((_, _, price) : xs) = price + calculateTotal xs

formatBill :: Int -> BillType -> String
formatBill discountAmount items =
    let header = "              Haskell Stores\n"
        formatItem (barcode, name, price) =
            name ++ replicate (35 - length name) '.' ++ " " ++ formatPrice price ++ "\n"
        formatPrice price =
            let dollars = floor price :: Int
                cents = round ((price - fromIntegral dollars) * 100) :: Int
                centsStr = if cents < 10 then "0" ++ show cents else show cents
            in show dollars ++ "." ++ centsStr
        itemLines = concat (map formatItem items)
        totalAmount = calculateTotal items - fromIntegral discountAmount
        discountLine = "Discount" ++ replicate 27 '.' ++ " " ++ show discountAmount ++ ".00\n"
        totalLine = "Total" ++ replicate 30 '.' ++ " " ++ formatPrice totalAmount ++ "\n"
    in header ++ itemLines ++ discountLine ++ totalLine

bill :: BillType
bill = [
    (1234, "Dry Sherry, 1lt", 5.40),
    (4719, "Fish Fingers", 1.21),
    (3814, "Orange Jelly", 0.56),
    (1112, "Hula Hoops (Giant)", 1.33),
    (1113, "Unknown Item", 0.00),
    (1234, "Dry Sherry, 1lt", 5.40)
    ]

printBill :: IO ()
printBill = do
    putStr (formatBill (makeDiscount (discountItemCount bill)) bill)

-- 7.2
listBS :: [Int] -> Int
listBS [] = 0
listBS [x] = x
listBS (x : y : _) = x + y

-- 7.3
firstPlusOne :: [Int] -> Int
firstPlusOne xs = if null xs then 0 else 1 + head xs

sumFirstTwo :: [Int] -> Int
sumFirstTwo xs
    | null xs = 0
    | null (tail xs) = head xs
    | otherwise = head xs + head (tail xs)

-- 7.4
product :: [Int] -> Int
product xs = foldr (*) 1 xs

-- 7.5
and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

-- 7.7
unique :: [Int] -> [Int]
unique xs = [x | x <- xs, count x xs == 1]
    where
        count x [] = 0
        count x (y : ys)
            | y == x = 1 + count x ys
            | otherwise = count x ys

-- 7.8
unzip :: [(Int, Int)] -> [Int]
unzip [] = []
unzip ((x, y) : ys) = [x] ++ [y] ++ unzip ys

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

-- 7.9
iSort :: [Int] -> [Int]
iSort [] = []
iSort [x] = [x]
iSort (x : xs) = insert (iSort xs)
    where
        insert [] = [x]
        insert (y : ys)
            | x < y = x : y : ys
            | otherwise = y : insert ys

merge :: [Int] -> [Int] -> [Int]
merge left [] = left
merge [] right = right
merge (l : left) (r : right)
    | l <= r = l : merge left (r : right)
    | otherwise = r : merge (l : left) right

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = let (left, right) = splitAt (length xs `div` 2) xs
               in merge (mergeSort left) (mergeSort right)

-- 7.14
drop :: Int -> [a] -> [a]
drop n [] = []
drop n xs | n <= 0 = xs
drop n (_ : xs) = drop (n-1) xs 

splitAt :: Int -> [a] -> ([a], [a])
splitAt n [] = ([], [])
splitAt n xs | n <= 0 = ([], xs)
splitAt n (x : xs) = 
    let (firstPart, secondPart) = splitAt (n-1) xs
    in if n > 0 then (x : firstPart, secondPart) else ([], x : xs)

-- 7.18
isSublist :: String -> String -> Bool
isSublist [] _ = True
isSublist _ [] = False
isSublist (x : xs) (y : ys)
    | x == y = isSublist xs ys
    | otherwise = isSublist (x : xs) ys

isSubsequence :: String -> String -> Bool
isSubsequence [] _ = True
isSubsequence _ [] = False
isSubsequence (x:xs) (y:ys)
    | x == y = isSubsequence xs ys
    | otherwise = isSubsequence (x : xs) ys

-- isSublist "ship" "Fish & Chips"
-- isSublist "hippies" "Fish & Chips"
-- isSubsequence "Chip" "Fish & Chips"
-- isSubsequence "Chin up" "Fish & Chips"

-- 7.25
isPalinBasic :: String -> Bool
isPalinBasic s = s == reverse s

isPalin :: String -> Bool
isPalin s = let ben = map toLower (filter isAlpha s)
            in ben == reverse ben

-- isPalin "Madam I'm Adam"

-- 7.26
subst :: String -> String -> String -> String
subst oldSub newSub [] = []
subst oldSub newSub st
    | take (length oldSub) st == oldSub = newSub ++ drop (length oldSub) st
    | otherwise = head st : subst oldSub newSub (tail st)

-- subst "much" "tall" "How much is that?"