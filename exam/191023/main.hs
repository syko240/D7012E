f :: [Int] -> Int
f = length.map (+7)

e = [] : []

f2 :: [a -> Bool] -> [[a] -> [a]]
f2 = map filter

isOrdered :: Ord a => [a] -> Bool
isOrdered [] = True
isOrdered [x] = True
isOrdered (x:y:xs) = x <= y && isOrdered (y:xs)

mkList :: Int -> Int -> [Int]
mkList f s = f : mkList s (s+(s-f))

data  TT a = Leaf | Node a (TT a) (TT a) (TT a) deriving (Show, Eq)

t1 :: Num a => TT a
t1 = Node 1
    (Node 2 Leaf Leaf Leaf)
    (Node 3
        (Node 4 Leaf Leaf Leaf)
        (Node 5 Leaf Leaf Leaf)
        (Node 6 Leaf Leaf Leaf))
    (Node 7 Leaf Leaf Leaf)

prune :: TT a -> TT a
prune (Node a Leaf Leaf Leaf) = Leaf
prune (Node a left mid right) =
    Node a (prune left) (prune mid) (prune right)
prune Leaf = Leaf

prune2 :: TT a -> Int
prune2 Leaf = 0
prune2 tree = 1 + prune2 (prune tree)

cashreg :: IO ()
cashreg = do cashreg' 0

cashreg' :: Int -> IO ()
cashreg' acc = do
    line <- getLine
    if line == "" then do 
        print acc
        putStrLn "-------"
        cashreg' 0
    else if line == "e" then do return ()
    else do
        cashreg' (acc + read line)