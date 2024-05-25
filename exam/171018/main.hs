tail2 :: [a] -> [a]
tail2 [] = []
tail2 (x:xs) = xs

init2 :: [a] -> [a]
init2 [] = []
init2 [x] = []
init2 (x:xs) = x : init2 xs

suffixes :: [a] -> [[a]]
suffixes [] = error "empty list"
suffixes [x] = [[x]]
suffixes xs = xs : suffixes (tail2 xs)

prefixes :: [a] -> [[a]]
prefixes [] = error "empty list"
prefixes [x] = [[x]]
prefixes xs = xs : prefixes (init2 xs)

data Tree a = Leaf a | Node (Tree a) (Tree a)

t = Node 
    (Node (Leaf 1) (Node (Node (Leaf 2) (Leaf 3)) (Leaf 4)))
    (Node (Leaf 5) (Node (Leaf 6) (Node (Leaf 7) (Leaf 8))))

sumTree :: Num a => Tree a -> a
sumTree (Leaf x) = x
sumTree (Node l r) = sumTree l + sumTree r

calculator :: IO ()
calculator = do calc []

calc :: [Int] -> IO ()
calc xs = do
    putStr "Enter number: "
    line <- getLine
    if line == "" then do
        if length xs == 0 then do
            putStrLn ("Numbers entered: (none)")
            putStrLn ("Accumulated sum: 0")
            putStrLn "Sum reset."
        else do
            putStr "Numbers entered: "
            pNum xs
            putStrLn ("Accumulated sum: " ++ show (sum xs))
            putStrLn "Sum reset."
        calc []
    else do calc (read line : xs)
    where
        pNum [x] = do putStrLn (show x)
        pNum (x:xs) = do
            putStr (show x ++ " ")
            pNum xs

evenCumSum :: Int -> Int
evenCumSum n = foldr (+) 0 (map (\x -> x * x * x) [x | x <- [2..n], x `mod` 2 == 0])