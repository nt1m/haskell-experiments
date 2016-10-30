binary :: Int -> [Int]
binary 0 = [0]
binary x = rem x 2 : binary (x `div` 2)

-- Nick's longer version
binary' :: Integral a => a -> [a]
binary' 0 = [0]
binary' n = r : binary' q
  where
    (q, r) = n `divMod` 2 -- Yay destructuring \o/

decimal :: Integer -> [Integer] -> Integer
decimal p [] = 0
decimal p (x:xs) = (p * x) + decimal (p * 2) xs

showBinary :: Int -> String
showBinary x = concat (map show (reverse (binary x)))

powers :: map (2 ^) [0 .. ]

zip :: [a] -> [b] -> [(a,b)]
zip [] ys = []
zip xs [] = []
zip (x:xs) (y:ys) = (x,y): zip xs ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> c
zipWith f [] ys = []
zipWith f xs [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys