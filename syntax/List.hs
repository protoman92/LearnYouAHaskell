import Data.List

searchSublist :: (Eq a) => [a] -> [a] -> Bool
searchSublist sl l =
    let len = length sl
        foldFn = (\x acc -> if take len x == sl then True else acc)
    in foldr foldFn False . tails $ l

-- concat
concat' :: [[a]] -> [a]
concat' = foldr (\x acc -> x ++ acc) []

-- intersperse
intersperse' :: a -> [a] -> [a]
intersperse' a aL =
    let aLength = length aL
        fistElement acc = null acc
        foldFn = (\x acc -> if fistElement acc then x:acc else x:a:acc)
    in foldr foldFn [] aL

-- intercalate
intercalate' :: [a] -> [[a]] -> [a]
intercalate' list = concat' . intersperse' list

-- foldl'
foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' _ b [] = b
foldl'' f b (x:xs) = foldl'' f (x `seq` f b x) xs

-- concatMap
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = concat' . map f

-- and
and' :: [Bool] -> Bool
and' = foldr (\x acc -> if not x then False else acc) True

-- or
or' :: [Bool] -> Bool
or' = foldr (\x acc -> if x then True else acc) False

-- any
any' :: (a -> Bool) -> [a] -> Bool
any' f = or . map f

-- all
all' :: (a -> Bool) -> [a] -> Bool
all' f = and . map f

-- iterate
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x:(iterate' f . f $ x)

-- splitAt
splitAt' :: (Integral a) => a -> [b] -> ([b], [b])
splitAt' _ [] = ([], [])
splitAt' index list
    | inx < 0 = ([], list)
    | inx >= len = (list, [])
    | otherwise = (x:xs', xs'')
    where len = length list
          inx = fromIntegral index
          (x:xs) = list
          (xs', xs'') = splitAt' (index - 1) xs

-- dropWhile
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f l@(x:xs)
    | f x = dropWhile' f xs
    | otherwise = l

-- span --
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' _ [] = ([], [])
span' f list@(x:xs)
    | f x = (x:xs', xs'')
    | otherwise = ([], list)
    where (xs', xs'') = span' f xs