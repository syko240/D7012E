prefix :: Int -> [a] -> [a]
prefix 0 xs = []
prefix i (x:xs)
    | i > length xs = x:xs
    | i < 0 = []
    | otherwise = x : prefix (i-1) xs

suffix :: Int -> [a] -> [a]
suffix 0 xs = []
suffix i xs
    | i > length xs = xs
    | i < 0 = []
    | otherwise = suffix (i-1) (init xs) ++ [last xs]

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)

t = Node
    (Node (Leaf 1) (Node (Node (Leaf 2) (Leaf 3)) (Leaf 4)))
    (Node (Leaf 5) (Node (Leaf 6) (Node (Leaf 7) (Leaf 8))))

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l r) = flatten l ++ flatten r

balance :: Tree a -> Tree a
balance tree = balance' (flatten tree)

balance' :: [a] -> Tree a
balance' [x] = Leaf x
balance' xs = Node (balance' (prefix (length xs `div` 2) xs))
    (balance' (suffix (length xs `div` 2) xs))

cashreg :: IO ()
cashreg = do cashreg' 0

cashreg' :: Int -> IO ()
cashreg' acc = do
    line <- getLine
    if line == "" then do
        print acc
        putStrLn "-------"
        cashreg' 0
    else if line == "e" then do
        return ()
    else do
        let acc2 = acc + read line
        cashreg' acc2

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs