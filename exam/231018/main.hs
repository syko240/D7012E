f1 :: t1 -> (t2 -> t1 -> t3) -> t2 -> t3
f1 x y z = y z x

f2 :: a -> [[a]] -> ([a], [[a]])
f2 a (b:bs) = (a:b, bs)

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys

merge_lists :: Ord t => [[t]] -> [t]
merge_lists xs = foldr (merge) [] xs

newt ::Float -> Float -> Float
newt a e = let x0 = a in loop (nextx x0) x0
    where
        nextx x = (1 / 3) * ((a / x^2) + 2*x)
        loop xi x = if abs (xi - x) < e then xi else loop (nextx xi) xi

data T = Leaf Int | Node Int T T

t :: T
t = Node 1
    (Node 2 (Node 3 (Leaf 4) (Leaf 5)) (Leaf 6))
    (Node 7 (Leaf 8) (Leaf 9))

onLevel :: T -> Int -> [Int]
onLevel tree 0 = [getVal tree]
onLevel (Leaf _) _ = []
onLevel (Node _ l r) n = onLevel l (n-1) ++ onLevel r (n-1)

getVal :: T -> Int
getVal (Leaf x) = x
getVal (Node x _ _) = x

slice :: [t] -> [Int] -> [[t]]
slice [] [] = []
slice xs (y:ys) = take y xs : slice (drop y xs) ys
slice _ _ = error "ben is cumming for you"

prod :: IO ()
prod = do 
    putStr "How many positive integers are to be multiplied? "
    line <- getLine
    prod' (read line) 1

prod' :: Int -> Int -> IO ()
prod' n acc = do
    putStr "Enter an integer: "
    line <- getLine
    let acc2 = acc * read line
    if n == 1 then do
        putStrLn ("Total product: " ++ show acc2)
        return ()
    else do
        prod' (n-1) acc2