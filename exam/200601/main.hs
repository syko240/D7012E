zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3' xs ys zs

negs :: [Int]
negs = (-1) : map (\x -> x - 1) negs

data Tree a = Leaf (Int, a) | Node a (Tree a) (Tree a)

isMaxHeap :: Ord a => Tree a -> Bool
isMaxHeap (Leaf _) = True
isMaxHeap (Node a left right) = a >= getVal left && a >= getVal right 
    && isMaxHeap left && isMaxHeap right
    where
        getVal (Node a _ _) = a
        getVal (Leaf v) = snd v


e3 :: (a -> b) -> [[a]] -> [[b]]
e3 = map.map

e5 :: [a -> b] -> [[a] -> [b]]
e5 = map map

e4 = [[] : [] : []] : []

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f xs = [x | x <- xs, f x]

lol :: (Int, Int, Int) -> IO ()
lol (s, m, g) = do
    putStrLn ("The game LOL starts. Player 1 says " ++ show s ++ ".")
    lol2 2 (s, m, g)

lol2 :: Int -> (Int, Int, Int) -> IO ()
lol2 p (s, m, g) = do
    putStr ("Your opponent said " ++ show s
        ++ ". What do you say, player " ++ show p ++ "?:")
    line <- getLine
    if read line >= g then do
        putStr "Goal reached! Player "
        if p == 1 then do
            putStrLn "2 wins! Game over!"
        else do
            putStrLn "1 wins! Game over!"
    else if read line < g && read line <= s+m && read line > s then do
        lol2 (if p == 1 then 2 else 1) (read line, m, g)
    else do
        putStrLn "Invalid answer - Try again!"
        lol2 p (s, m, g)