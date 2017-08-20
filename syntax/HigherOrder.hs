applyTwice :: (a -> a) -> a -> a
applyTwice func a = func (func a)

guardZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
guardZipWith f aL bL
    | null aL = []
    | null bL = []
    | otherwise = [f a b] ++ guardZipWith f axs bxs
    where (a:axs) = aL
          (b:bxs) = bL

patternZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
patternZipWith _ [] _ = []
patternZipWith _ _ [] = []
patternZipWith f (a:axs) (b:bxs)= [f a b] ++ patternZipWith f axs bxs

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f a b = f b a

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f aL = [f a | a <- aL]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = (if f x then [x] else []) ++ filter' f xs

quickSortFilter :: (Ord a) => [a] -> [a]
quickSortFilter [] = []
quickSortFilter (x:xs) = qs1 ++ [x] ++ qs2
    where l1 = filter' (<= x) xs
          l2 = filter' (> x) xs
          qs1 = quickSortFilter l1
          qs2 = quickSortFilter l2

lgDivBy3829 :: (Integral a) => a -> a
lgDivBy3829 0 = 0
lgDivBy3829 a = head (f3829 [a,a-1..0])
    where lg3829 = (>= 3829)
          divBy3829 x = x `rem` 3829 == 0
          fLg = filter' (>= 3829)
          fDiv = filter' divBy3829
          f3829 aL = fDiv (fLg aL)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | not (f x) = []
    | otherwise = [x] ++ takeWhile' f xs

oddSquareSmaller :: (Integral a) => a -> a
oddSquareSmaller a = sum (takeWhileLta (filterOdd (mapSquared [0,1..])))
    where squared = (^ 2)
          lta = (< a)
          mapSquared = map' squared
          filterOdd = filter' odd
          takeWhileLta = takeWhile' lta

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz x = (x:f x)
    where f x1
            | odd x1 = collatz (x1 * 3 + 1)
            | even x1 = collatz (x1 `div` 2)

filterCollatz :: (Integral a) => a -> a -> Int
filterCollatz x y
    | x >= y = 0
    | otherwise = length (filterG15 (map' length (map' collatz [x,x+1..y])))
        where gt15 = (> 15)
              filterG15 = filter' gt15

elemFoldl :: (Eq a) => a -> [a] -> Bool
elemFoldl x xs = foldl (\acc a -> if a == x then True else acc) False xs

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\x acc -> (f x:acc)) []

maximumFoldr :: (Ord a, Num a) => [a] -> a
maximumFoldr = foldr (\x acc -> if x > acc then x else acc) 0

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [0,1..]