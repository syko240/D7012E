f1 :: (t1 -> t2) -> t1 -> t2
f1 x y = x y

f2 :: Ord a => a -> a -> Bool
f2 x y = x < y

f3 :: t1 -> ((t2 -> t1 -> t3) -> t2) -> (t2 -> t1 -> t3) -> t3
f3 a b c = c (b c) a

incFrom :: Int -> [Int]
incFrom n = n : map (+1) (incFrom n)

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

data Tree = Leaf Int | Node Int Tree Tree deriving (Eq, Show)

t = Node (-4)
    (Node 6 (Leaf (-2)) (Node (-3) (Leaf 2) (Leaf (-7))))
    (Leaf 1)

minTree :: Tree -> (Tree, Int)
minTree tree = getMin (perm tree)
    where
        getMin [t] = (t, inorder t)
        getMin (t1:t2:ts) = if inorder t1 < inorder t2 then
            getMin (t1:ts)
            else getMin (t2:ts)

inorder :: Tree -> Int
inorder (Leaf x) = x
inorder (Node x l r) = x + inorder l + inorder r

perm :: Tree -> [Tree]
perm (Leaf x) = [Leaf x]
perm (Node x l r) = Node x l r : perm l ++ perm r

repeats :: Eq a => [a] -> Bool
repeats [] = False
repeats xs = checkPrefixes xs 1
  where
    checkPrefixes xs len
      | len > length xs `div` 2 = False
      | repeats' xs (take len xs) = True
      | otherwise = checkPrefixes xs (len + 1)

repeats' :: Eq a => [a] -> [a] -> Bool
repeats' [] _ = True
repeats' xs patrn
    | null patrn = False
    | length xs < length patrn = False
    | otherwise = (take (length patrn) xs == patrn) 
        && repeats' (drop (length patrn) xs) patrn
