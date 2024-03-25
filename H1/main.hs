import Data.Char (ord,chr)

smallestKSets :: Int -> [Int] -> [[Int]]
smallestKSets _ [] = error "list is empty"
smallestKSets k xs = error "tbc"

main :: IO ()
main = do
    let k = 3 :: Int
    -- let list = [-1, 2, -3, 4, -5] :: [Int]
    let list = [] :: [Int]

    print ( smallestKSets k list )