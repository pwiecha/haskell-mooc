module Ch1 where

polynomial :: Double -> Double 
polynomial x = x^2 - x - 1

sq_rt :: Double -> Double
sq_rt x = x**(0.5) -- ** : exponentiation w double

sq_rt2 :: Double -> Double
sq_rt2 x = sqrt x

concatenate :: [Char] -> [Char] -> [Char]
concatenate a b = a ++ b

circleCircum :: Double -> Double
circleCircum r = 2 * pi * r
    where pi = 3.1415


fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

fact2 :: Int -> Int
fact2 n = if n == 0 then 1 else n * fact2 (n-1)

-- with guard
fact3 :: Int -> Int
fact3 n
    | n > 1 = n * fact3 (n-1)
    | otherwise = 1

addd :: Double -> Double -> Double
addd x y = sq_rt x + sq_rt y
