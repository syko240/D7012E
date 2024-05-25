isOrdered' :: Ord a => [a] -> Bool
isOrdered' [] = error "empyt list"
isOrdered' [x] = True
isOrdered' (x:y:xs) = x <= y && isOrdered' (y:xs)

compress :: [Int] -> [(Int,Int)]
compress [] = []
compress xs = helper (compress1 xs)

helper :: [(Int, Int)] -> [(Int, Int)]
helper [] = []
helper [(a,b)] = [(a,b)]
helper (x:y:xs)
    | fst y - snd x <= 1 = helper ((fst x, snd y):xs)
    | otherwise = x : helper (y:xs)

compress1 :: [Int] -> [(Int,Int)]
compress1 [] = []
compress1 [x] = [(x, x)]
compress1 [x, y] = if y - x <= 1 then [(x, y)] else [(x,x),(y,y)]
compress1 (x:y:z:xs)
    | y - x <= 1 && z - y <= 1 = (x, z) : compress1 xs
    | y - x <= 1 = (x, y) : compress1 (z:xs)
    | otherwise = (x, x) : compress1 (y:z:xs)

mkList :: Int -> Int -> [Int]
mkList f s = f : mkList s (s + (s-f))

data Tree a = Leaf | Node (a, Int) (Tree a) (Tree a) (Tree a) deriving (Eq, Show)

extract :: Int -> Tree a -> [a]
extract i Leaf = []
extract i (Node x l m r) = if i == snd x then
    fst x : extract i l ++ extract i m ++ extract i r
    else extract i l ++ extract i m ++ extract i r

exampleTree :: Tree Int
exampleTree = Node (1, 1) (Node (2, 2) (Node (3, 3) Leaf Leaf Leaf) (Node (4, 2) Leaf Leaf Leaf) (Node (5, 5) Leaf Leaf Leaf)) (Node (6, 6) (Node (7, 7) Leaf Leaf Leaf) (Node (8, 8) Leaf Leaf Leaf) (Node (9, 9) Leaf Leaf Leaf)) (Node (10, 10) (Node (11, 11) Leaf Leaf Leaf) (Node (12, 12) Leaf Leaf Leaf) (Node (13, 13) Leaf Leaf Leaf))

game :: IO ()
game = do
    putStrLn "Welcome to the Game!"
    putStrLn "What secret number between 1 and 100 am I thinking of?!" 
    game' 20 1

game' :: Int -> Int -> IO ()
game' x nr = do
    putStr ("Enter guess number " ++ show nr ++ ": ")
    line <- getLine
    if read line == x then do
        putStrLn ("Correct after " ++ show nr ++ " guesses!")
        putStrLn "Thank's for playing!"
        return ()
    else if read line < x then do
        putStrLn "Too low!"
        game' x (nr+1)
    else if read line > x then do
        putStrLn "Too high!"
        game' x (nr+1)
    else do
        putStrLn "Invalid Input!"
        game' x nr