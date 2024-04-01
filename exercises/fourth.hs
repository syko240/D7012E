-- 12.2
numEqual :: Int -> [Int] -> Int
numEqual x (y : ys)
    | null ys = if x == y then 1 else 0
    | x == y = 1 + numEqual x ys
    | otherwise = numEqual x ys

-- (Int -> [Int] -> Int)



-- 12.3