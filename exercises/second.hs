import Prelude hiding (elem)

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