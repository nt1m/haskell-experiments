import Data.List

isPrime :: Int -> Bool
isPrime x
  | x < 2 = False
  | otherwise = all (isNotDivisible x) [2..x-1]

isDivisible :: Int -> Int -> Bool
isDivisible x 0 = False
isDivisible x y = rem x y == 0

isNotDivisible :: Int -> Int -> Bool
isNotDivisible x y = not$isDivisible x y

writeAsProductOfPrimes :: Int -> [Int]
writeAsProductOfPrimes 1 = [1]
writeAsProductOfPrimes x = prime : writeAsProductOfPrimes' (x `div` prime)
  where
    prime = findFirstPrimeDivisor x

prodPrimes :: Int -> String
prodPrimes x = intercalate " x " (map show$writeAsProductOfPrimes x)

findFirstPrimeDivisor :: Int -> Int
findFirstPrimeDivisor x
  | isPrime x = x
  | otherwise = [n | n <- [1..x-1], isPrime n, isDivisible x n]!!0

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib' :: Int -> Int
fib' n
  | n == 0    = 1
  | n == 1    = 1
  | otherwise = fib' (n - 1) + fib' (n - 2)

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

fib'' :: Int -> Integer
fib'' n = fibs !! n
