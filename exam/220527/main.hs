ints = 0: map(\x -> if x >= 0 then ((x * (-1)) - 1) else (x * (-1))) ints

foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 _ y [] = y
foldr2 f y (x:xs) = f x (foldr2 f y xs)

data RatTree = Leaf | Node (Int, Int) RatTree RatTree deriving Show

calkinwilf :: Int -> RatTree
calkinwilf n = tree (1,1) n

tree :: (Int, Int) -> Int -> RatTree
tree (a, b) n
    | a > n || b > n = Leaf
    | otherwise = Node (a, b) left right
    where
        left = tree (a, a+b) n
        right = tree (a+b, b) n

concat' :: [[a]] -> [a]
concat' [x] = x
concat' ((x:y):xs) = x : concat (y:xs)

concat2 :: [[[a]]] -> [a]
concat2 [] = []
concat2 [[x]] = x
concat2 (x:xs) = concat' x ++ concat2 xs

concat3 :: [[[[a]]]] -> [a]
concat3 [] = []
concat3 [[[x]]] = x
concat3 (x:xs) = concat2 x ++ concat3 xs