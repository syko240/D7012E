import Prelude hiding (compare)

-- 12.2
numEqual :: Eq a => [a] -> a -> Int
numEqual xs x = length $ filter (== x) xs
-- takes a list of any type that can be chacked for equality (Eq a), returning an Int

member :: Eq a => [a] -> a -> Bool
member xs x = numEqual xs x > 0

-- 12.3
oneLookupFirst :: Eq a => [(a,b)] -> a -> Maybe b
oneLookupFirst xs x = case filter ((== x) . fst) xs of
                        ((_,y) : _) -> Just y
                        [] -> Nothing

oneLookupSecond :: Eq b => [(a,b)] -> b -> Maybe a
oneLookupSecond xs x = case filter ((== x) . snd) xs of
                          ((y,_) : _) -> Just y
                          [] -> Nothing

-- 12.5
class Visible a where
    toString :: a -> String
    size :: a -> Int

instance Visible Int where
    toString = show
    size n = length . show $ abs n

-- 12.6
-- compare x y = size x <= size y
-- compare works with instances of 'Visible' from 12.5
-- Visible a => a -> a -> Bool
compare :: Visible a => a -> a -> Bool
compare x y = size x <= size y

-- compare (1 :: Int) (2 :: Int)

-- 12.8 (bs)
instance (Ord a, Ord b) => Ord (a, b)
    where
        compare (x1, y1) (x2, y2) = compare x1 x2 <> compare y1 y2

instance Ord b => Ord [b] where
    compare [] [] = EQ
    compare [] _  = LT
    compare _  [] = GT
    compare (x : xs) (y : ys) =
        case compare x y of
          EQ -> compare xs ys
          other -> other