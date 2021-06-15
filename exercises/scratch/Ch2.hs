module Ch2 where

fibonacci :: Integer -> Integer
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- w/ helper function
-- naming alternative version of function with apostrophe (f prime)
fibonacciEfficient :: Integer -> Integer
fibonacciEfficient n = fibonacciEfficient' 0 1 n

fibonacciEfficient' :: Integer -> Integer -> Integer -> Integer
fibonacciEfficient' a b 1 = b
fibonacciEfficient' a b n = fibonacciEfficient' b (a+b) (n-1)

-- factorial implemented w/ guards
factorialGuards :: Integer -> Integer
factorialGuards n
    | n < 0 = -1
    | n < 1 = 1
    | otherwise = n * factorialGuards (n-1)
