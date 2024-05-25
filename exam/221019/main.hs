evenNat :: [Int]
evenNat = 0 : map (+2) evenNat

hamming :: Eq t => [t] -> [t] -> Int
hamming xs ys
    | (length xs) == (length ys) = h xs ys
    | otherwise = error "not of equal length"

h :: Eq t => [t] -> [t] -> Int
h [] [] = 0
h (x:xs) (y:ys)
    | x /= y = 1 + (h xs ys)
    | otherwise = h xs ys

minHamming :: Eq t => [t] -> [t] -> Int
minHamming xs ys = min' (minHamming' xs ys)

min' :: [(Int, Int)] -> Int
min' [x] = snd x
min' (x:y:xs) = if fst x < fst y then min' (x:xs) else min' (y:xs)

minHamming' :: Eq t => [t] -> [t] -> [(Int, Int)]
minHamming' xs ys = [(hamming (take (length ys) (drop i xs)) ys, i) | i <- [0..(length xs - length ys)]]