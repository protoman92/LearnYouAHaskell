foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ x0 [] = x0
foldr' f x0 (x:xs) = f x (foldr' f x0 xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ x0 [] = x0
foldl' f x0 (x:xs) = foldl' f (f x0 x) xs