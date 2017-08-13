-- Which right triangle that has integers for all sides and all sides equal to 
-- or smaller than 10 has a perimeter of 24?
rtpr :: [Int] -> Int -> [(Int, Int, Int)]
rtpr rng pr = [(a, b, c) |
    c <- rng, b <- rng, a <- rng,
    a^2 + b^2 == c^2, a + b + c == pr]

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1)

fibonacci :: (Integral a) => a -> a
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bmiRespond :: (RealFloat a) => a -> String
bmiRespond bmi
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Overweight"
    | otherwise = "Obese"

bmiCalculateAndRespond :: (RealFloat a) => a -> a -> String
bmiCalculateAndRespond weight height
    | bmi <= underweight = "Underweight"
    | bmi <= normal = "Normal"
    | bmi <= overweight = "Overweight"
    | otherwise = "Obese"
    where bmi = weight / height ^ 2
          underweight = 18.5
          normal = 25.0
          overweight = 30.0

initials :: String -> String -> String
initials first last = [f] ++ "." ++ [l]
    where (f:_) = first
          (l:_) = last