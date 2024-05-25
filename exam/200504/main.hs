zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith2 f (x:xs) (y:ys) = f x y : zipWith2 f xs ys
zipWith2 _ _ _ = []

add :: [Int] -> [Int] -> [Int]
add = zipWith2 (\x y -> x + y)

sub :: [Int] -> [Int] -> [Int]
sub = zipWith2 (\x y -> x - y)

data Tree a = Leaf (Int, a) | Node Int (Tree a) (Tree a)

isWellBuilt :: Tree a -> Bool
isWellBuilt tree = abs (maxd tree - mind tree) <= 1
    where
        maxd (Leaf (_,_)) = 1
        maxd (Node _ left right) = 1 + max' (maxd left) (maxd right)
        max' a b = if a > b then a else b
        mind (Leaf (_,_)) = 1
        mind (Node _ left right) = 1 + min' (mind left) (mind right)
        min' a b = if a > b then b else a

t1 = Node 2 (Leaf (1,1)) (Node 4 (Leaf (3,3)) (Leaf (5,5)))
t2 = Node 3 t1 t1
t3 = Node 6 t1 t2

map' f xs = [f x | x <- xs]

while :: (t -> Bool) -> (t -> t) -> t -> t
while condition next state
    | condition state = while condition next (next state)
    | otherwise = state

repeat :: String -> Int -> String
repeat s 0 = ""
repeat s n = s ++ while () () 
