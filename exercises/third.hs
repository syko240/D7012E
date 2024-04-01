-- 9.2
length' :: [Int] -> Int
length' xs = sum $ map (const 1) xs

-- 9.4
-- map f (map g xs)

-- f :: a -> c
-- g :: b -> a

-- xs :: [b]
-- map g xs :: [a]
-- map f (map g xs) :: [c]

-- 9.6
listOfSquares :: [Int] -> [Int]
listOfSquares (x : xs)
    | null xs = [x^2]
    | otherwise = (x^2) : listOfSquares xs

sumOfSquares :: [Int] -> Int
sumOfSquares xs = sum $ listOfSquares xs

greaterThenZero :: [Int] -> Bool
greaterThenZero (x : xs)
    | null xs = x > 0
    | otherwise = x > 0 && greaterThenZero xs

-- 9.7
f :: Int -> Int
f x
    | x < 3 = x + 1
    | otherwise = 3

minValue :: (Int -> Int) -> Int -> Int
minValue f 0 = f 0
minValue f n = min (f n) (minValue f (n-1))

equalValue :: (Int -> Int) -> Int -> Bool
equalValue f 0 = True
equalValue f n = f n == f (n-1) && equalValue f (n-1)

greaterAndAscending :: (Int -> Int) -> Int -> Bool
greaterAndAscending f 0 = True
greaterAndAscending f n = let p = f (n-1) in 0 < p && p < f n && greaterAndAscending f (n-1)

-- 9.9
iter :: Int -> (Int -> Int) -> Int -> Int
iter 0 f x = x
iter n f x = iter (n - 1) f (f x)

-- 9.10
powerN :: Int -> Int
powerN n = iter n double 1
    where
        double x = 2 * x

-- 9.11
sumOfSquares' :: Int -> Int
sumOfSquares' n = foldr (+) 0 $ map square [1..n]
    where
        square x = x * x

-- 9.16
p :: Int -> Bool
p x = x == 10

filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ [] = []
filterFirst p (x : xs)
    | not (p x) = x : filterFirst p xs
    | otherwise = xs

-- 9.17
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p xs = reverse $ filterFirst p $ reverse xs

-- 10.3
composeFunList :: [a -> a] -> (a -> a)
composeFunList (f : fs)
    | null fs = f
    | otherwise = f . composeFunList fs

-- 10.7
flip :: (a -> b -> c) -> (b -> a -> c)
flip f = (\a b -> f b a)

-- 10.8
notWhitespace :: Char -> Bool
notWhitespace = \c -> not (c `elem` " \t\n")

-- 10.13
sec :: [Int] -> [Int]
sec = map (\a -> a + 1) . filter (\b -> b > -1)

-- 10.14
type Picture = IO ()

chessBoard :: Int -> Picture
chessBoard n
    | n <= 0 = putStr ""
    | otherwise = drawBoard n n

drawBoard :: Int -> Int -> IO ()
drawBoard _ 0 = return ()
drawBoard n i = do
    drawLine n i
    drawBoard n (i - 1)

drawLine :: Int -> Int -> IO ()
drawLine n i = putStrLn [if even (row + col) then '#' else ' ' | col <- [1..n], _ <- [1..2]]
    where row = n - i + 1

-- 10.22
compactCathedralEntry :: [Int] -> String
compactCathedralEntry pages = "cathedral: " ++ formatRanges (toRanges pages)
  where
    toRanges :: [Int] -> [Range]
    toRanges [] = []
    toRanges (x : xs) = go (x, x) xs
      where
        go current [] = [current]
        go (start, end) (y : ys)
          | y == end + 1 = go (start, y) ys
          | otherwise = (start, end) : go (y, y) ys
    
    formatRanges :: [Range] -> String
    formatRanges ranges = drop 2 $ foldl (\acc r -> acc ++ ", " ++ formatRange r) "" ranges

    formatRange :: Range -> String
    formatRange (start, end)
      | start == end = show start
      | otherwise = show start ++ "-" ++ show end

type Range = (Int, Int)

-- putStrLn $ compactCathedralEntry [3, 5, 6, 7, 9, 10]