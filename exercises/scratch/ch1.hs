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

