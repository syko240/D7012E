d :: [Char]
d = (++) "D70" "12E"

nat :: [Int]
nat = 0 : map (1+) nat

place :: [[(Int,Float)]] -> Float -> (Int, Float) -> [[(Int, Float)]]
place [] _ sausage = [[sausage]]
place (x:xs) w sausage
    | (mySum x + snd sausage) <= w = (sausage:x) : xs
    | otherwise = x : place xs w sausage

mySum :: [(Int, Float)] -> Float
mySum [] = 0
mySum (x:xs) = snd x + mySum xs

process :: [[(Int, Float)]] -> [(Int, Float)] -> Float -> [[(Int, Float)]]
process boxes [] _ = boxes
process boxes (x:xs) w = process (place boxes w x) xs w

scan :: Float -> [(Int, Float)] -> [[(Int, Float)]]
scan _ [] = [[]]
scan w (x:xs) = process [[x]] xs w

calc :: IO ()
calc = calc2 0

calc2 :: Int -> IO ()
calc2 acc = do
    putStrLn("acc=" ++ show(acc))
    putStr(">")
    line <- getLine
    if null line then do
        putStrLn("Bye")
        return ()
    else if line == "+" then do
        putStr("num?:")
        num <- getLine
        let result = (read num) + acc
        calc2 result
    else if line == "-" then do
        putStr("num?:")
        num <- getLine
        let result = acc - (read num)
        calc2 result
    else do
        putStrLn("Illegal input")
        calc2 acc