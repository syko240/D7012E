import Data.Char (isAlpha, toLower)
import Text.Read (readMaybe)
import Prelude hiding (repeat)

getNput :: IO ()
getNput = do
  line <- getLine
  putStrLn line

getInt :: IO Int
getInt = do
  line <- getLine
  return (read line :: Int)

put4times :: String -> IO ()
put4times str = do
  putStrLn str
  putStrLn str
  putStrLn str
  putStrLn str

-- 18.1
isPalinCheck :: IO ()
isPalinCheck = do 
    line <- getLine
    putStrLn $ show $ isPalin line

isPalin :: String -> Bool
isPalin s = let ben = map toLower (filter isAlpha s)
            in ben == reverse ben

-- 18.2
returnSum :: IO ()
returnSum = do
    line1 <- getInt
    line2 <- getInt
    print (line1 + line2)

-- 18.3
sumN :: IO ()
sumN = do
    n <- getInt
    sumN' n 0

sumN' :: Int -> Int -> IO ()
sumN' 0 sum = print sum
sumN' n sum = do
    line <- getInt
    sumN' (n-1) (sum+line)

-- 18.4
palinBenus :: IO ()
palinBenus = do
    putStrLn "Ben has commanded you to drop ur pants:"
    loop
        where
            loop = do
                line <- getLine
                if null line then putStrLn "Ben is satisfied."
                else do
                    putStrLn $ show $ isPalin line
                    loop

-- 18.5
merge :: [Int] -> [Int] -> [Int]
merge left [] = left
merge [] right = right
merge (l : left) (r : right)
    | l <= r = l : merge left (r : right)
    | otherwise = r : merge (l : left) right

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = let (left, right) = splitAt (length xs `div` 2) xs
               in merge (mergeSort left) (mergeSort right)

sortBenus :: IO ()
sortBenus = do
    putStrLn "Ben has commanded you to drop ur pants:"
    loop []
        where
            loop xs = do
                line <- getLine
                if null line then print (mergeSort xs)
                else case readMaybe line of
                    Just num -> loop (xs ++ [num])
                    Nothing -> loop xs

-- 18.6
mapF :: (a -> b) -> IO a -> IO b
mapF f action = do
    result <- action
    return (f result)

-- 18.7
repeat :: IO Bool -> IO () -> IO ()
repeat test oper = do
    result <- test
    if result
        then return ()
        else do
            oper
            repeat test oper