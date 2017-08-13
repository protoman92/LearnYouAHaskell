sum' :: (Num a) => [a] -> a
sum' items
    | null items = 0
    | otherwise = x + sum' xs
    where (x:xs) = items

length' :: [a] -> Int
length' items = sum' [1 | _ <- items]

replicate' :: (Ord i, Integral i) => a -> i -> [a]
replicate' x times
    | times == 0 = []
    | otherwise = x:replicate' x (times - 1)

take' :: (Integral i) => [a] -> i -> [a]
take' items count
    | count == 0 = []
    | count <= length = x:take' xs (count - 1)
    | otherwise = items
    where (x:xs) = items
          length = fromIntegral (length' items)

reverse' :: [a] -> [a]
reverse' items
    | null items = []
    | otherwise = reverse' xs ++ [x]
    where (x:xs) = items

repeat' :: a -> [a]
repeat' item = item:repeat' item

zip' :: [a] -> [b] -> [(a, b)]
zip' first second
    | fLength == 0 = []
    | sLength == 0 = []
    | otherwise = let
                    (x:xs) = first
                    (y:ys) = second
                  in
                    (x, y):zip' xs ys
    where fLength = length' first
          sLength = length' second

elem' :: (Eq a) => a -> [a] -> Bool
elem' item items
    | null items = False
    | item == x = True
    | otherwise = elem' item xs
    where (x:xs) = items

quickSort' :: (Ord a) => [a] -> [a]
quickSort' items
    | null items = []
    | otherwise = quickSort' a ++ [x] ++ quickSort' b
    where (x:xs) = items
          a = [y | y <- xs, y <= x]
          b = [y | y <- xs, y > x]

-- Tests --
testReplicate :: (Num a) => a -> Int -> [a]
testReplicate item count = [x * 2 |
                            let items = replicate' item count,
                            x <- items]