test1 :: [Char]
test1 = tail "7"


--test2 = \h -> 'h' 0
test3 :: [a -> b] -> [[a] -> [b]]
test3 = map map

middle :: [a] -> a
middle [] = error "cock"
middle [x] = x
middle [x, y] = x
middle xs = middle (drop 1 (take (length xs - 1) xs))
-- middle xs = let len = length xs in head (drop ((len) `div` 2) xs)

qsort :: Ord t => [t] -> [t]
qsort [] = []
qsort xs = qsort left ++ mid ++ qsort right
    where
        mid' = middle xs
        left = [x | x <- xs, x < mid']
        mid = [x | x <- xs, x == mid']
        right = [x | x <- xs, x > mid']

mkList :: Int -> Int -> [Int]
mkList f s = f : mkList s (s+(s-f))

data ThreeTree a = Leaf | Node (a, Int) (ThreeTree a)
    (ThreeTree a) (ThreeTree a)

t = Node ('X',1)
          (Node ('H',2)
                Leaf
                Leaf
                Leaf
          )
          Leaf
          (Node ('Z',4)
                (Node ('Y',3)
                      Leaf
                      Leaf
                      Leaf
                     )
                (Node ('J',2)
                      Leaf
                      Leaf
                      Leaf
                     )
                Leaf
          )

topPart :: Int -> ThreeTree a -> [a]
topPart 0 _ = []
topPart i Leaf = []
topPart i (Node (a, b) left mid right) =
    a : (topPart (i-1) left ++ topPart (i-1) mid ++ topPart (i-1) right)

calc :: IO ()
calc = do
    putStr "Enter number of integers to add: "
    line <- getLine
    calc2 0 (read line)

calc2 :: Int -> Int -> IO()
calc2 sum 0 = do putStrLn ("Total sum: " ++ show sum)
calc2 sum t = do
    putStr "Enter an integer: "
    line <- getLine
    calc2 (sum + read line) (t-1)