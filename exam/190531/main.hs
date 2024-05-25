totalSum :: [Int] -> Int
totalSum = foldr (+) 0

f2 :: [b -> c] -> [(a -> b) -> a -> c]
f2 = map (.)

f3 :: ((t1 -> t2) -> t1 -> t2) -> (t1 -> t2) -> t1 -> t2
f3 f x y = f (f x) y

balls :: [[a]] -> [[a]]
balls ([] : _) = []
balls x = map head x : balls (map tail x)

tr :: [[a]] -> [[a]]
tr [] = []
tr (x:xs) = [head x] : tr (xs)

triangle :: Int -> IO ()
triangle n = do
    row n
    if n == 1 then do return ()
    else do triangle (n-1)

row :: Int -> IO ()
row n = do
    if n == 1 then do
        putStrLn "*"
    else do
        putStr "*"
        row (n-1)


merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | x == y = x : merge xs ys
    | x > y = y : merge (x:xs) ys

merge3 :: [Int] -> [Int] -> [Int] -> [Int]
merge3 xs ys zs = merge xs (merge ys zs)

ham :: [Int]
ham = 1 : merge3 (map (2*) ham) (map (3*) ham) (map (5*) ham)