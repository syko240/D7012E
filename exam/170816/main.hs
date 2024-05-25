append :: [a] -> [a] -> [a]
append (x:xs) ys = x : append xs ys
append [] (y:ys) = y : append [] ys
append [] [] = []

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 [x] = x
concat1 (x:y:xs) = concat1 ((append x y):xs)

data TT a = Leaf | Node a (TT a) (TT a) (TT a) deriving (Eq, Show)

t :: TT Int
t = Node 1
    (Node 2 Leaf Leaf Leaf)
    (Node 3 
        (Node 4 Leaf Leaf Leaf) 
        (Node 5 Leaf Leaf Leaf) 
        (Node 6 Leaf Leaf Leaf))
    (Node 7 Leaf Leaf Leaf)

prune :: TT a -> TT a
prune Leaf = Leaf
prune (Node _ Leaf Leaf Leaf) = Leaf
prune (Node a l m r) = Node a (prune l) (prune m) (prune r)

pruneCount :: TT a -> Int
pruneCount Leaf = 0
pruneCount tree = 1 + pruneCount (prune tree)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x]

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 f (x:xs)
    | f x = x : filter2 f xs
    | otherwise = filter2 f xs

squareSum :: Int -> Int
squareSum n = foldr (+) 0 (map (\x -> x*x) [1..n])