f :: (a, (b,b)) -> b
f = fst.snd

f1 :: Int -> [a] -> Int
f1 a b = length b + a

f2 :: (t1 -> t2) -> t1 -> t2
f2 p q = p q

f3 x z y = z.x.y

zenon :: [Double]
zenon = (1 / 2) : map (/2) zenon

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f n [] = n
foldr' f n (x:xs) = f x (foldr' f n xs)

data ATree = LeafA Int | NodeA [BTree] ATree ATree
data BTree = LeafB String | NodeB [ATree] BTree BTree

exampleATree :: ATree
exampleATree =
    NodeA
        [ LeafB "abc"
        , NodeB
            [ LeafA 2
            , NodeA
                [ LeafB "defg"
                , NodeB
                    [ LeafA 1
                    , LeafA 3
                    ] 
                    (LeafB "hij")
                    (LeafB "kl")
                ]
                (LeafA 4)
                (LeafA 5)
            ]
            (LeafB "mnop")
            (LeafB "qr")
        ]
        (LeafA 10)
        (LeafA 20)

treesum :: ATree -> Int
treesum (LeafA x) = x
treesum (NodeA bs l r) = 
    sum (map (treesum') bs) + treesum l + treesum r

treesum' :: BTree -> Int
treesum' (LeafB s) = length s
treesum' (NodeB as l r) = 
    sum (map (treesum) as) + treesum' l + treesum' r

numMaxima :: [Int] -> [Int]
numMaxima (x:y:xs) = if x > y then x : numMaxima' (y:xs) 
    else numMaxima' (x:y:xs)
numMaxima _ = error "atleast 2 elements"

numMaxima' :: [Int] -> [Int]
numMaxima' [] = []
numMaxima' [x] = []
numMaxima' [x,y] = if y > x then [y] else []
numMaxima'(x:y:z:xs) = if y > x && y > z then y : numMaxima' (z:xs)
    else numMaxima' (y:z:xs)

equal :: [Int] -> [Int] -> Bool
equal xs ys = numMaxima xs == numMaxima ys

platous :: Eq t => [t] -> Int
platous (x:y:xs) = if x == y then 1 + platous (platous' (x:y:xs))
    else platous (y:xs)
    where
        platous' [] = []
        platous' [x] = []
        platous' (x:y:xs)
            | x == y = platous' (y:xs)
            | otherwise = (y:xs)
platous _ = 0