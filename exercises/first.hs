-- 3.7
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent m n p = (m /= n) && (n /= p) && (p /= m)

-- 3.8
threeEqual :: Int -> Int -> Int -> Bool
threeEqual m n p = (m==n) && (n==p)

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual x y z k = (threeEqual x y z) && (threeEqual y z k)

-- 3.15
numberNDroots :: Float -> Float -> Float -> Int
numberNDroots a b c
    | discriminant > 0 = 2
    | discriminant == 0 = 1
    | otherwise = 0
    where discriminant = b^2 - (4 * a * c)

-- 2 roots: 1 5 6
-- 1 root: 1 (-2) 1
-- no real root: 1 2 3

-- 3.16
numberRoots :: Float -> Float -> Float -> Int
numberRoots a b c
    | a == 0 && b == 0 && c == 0 = 3
    | discriminant > 0 = 2
    | discriminant == 0 = 1
    | otherwise = 0
    where discriminant = b^2 - (4 * a * c)

-- 3.17
smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c = (((-1) * b) - sqrt (b^2 - (4 * a * c))) / (2 * a)

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c = (((-1) * b) + sqrt (b^2 - (4 * a * c))) / (2 * a)

roots :: Float -> Float -> Float -> [Float]
roots a b c
    | a == 0 && b == 0 && c == 0 = []
    | discriminant < 0 = []
    | otherwise = [smallerRoot a b c, largerRoot a b c]
    where discriminant = b^2 - (4 * a * c)

-- 4.7
addMult :: Int -> Int -> Int
addMult a b
    | b > 0 = a + addMult a (b-1)
    | otherwise = 0

-- 4.8

-- 4.9
f :: Int -> Int
f x = x * 2 - 24

maxf :: Int -> Int
maxf n
    | n == 0 = max (f 0) n
    | otherwise = max (f n) (maxf (n-1))

-- 4.14
powerN :: Int -> Float
powerN 0 = 1
powerN n
    | n `mod` 2 == 0 = let m = powerN (n `div` 2) in m * m
    | otherwise = let m = powerN ((n - 1) `div` 2) in m * m * 2