{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}
module MathExtras where
import qualified GHC.Show

class Equation a where
  differentiate :: a -> a
  differentiate f = undefined
  integrate :: a -> a
  integrate f = undefined

type Affine = (Float, Float)
type Polynomial = [Float]

showPolynomial :: Polynomial -> String
showPolynomial [] = "0"
showPolynomial (0:xs) = showPolynomial xs
showPolynomial (x:xs) = 
  if l == 0 then
    (show x)
  else if l == 1 then
    (show x) ++ "x" ++ "+" ++ showPolynomial xs
  else
    (show x) ++ "x^" ++ show l ++ "+" ++ showPolynomial xs
  where
    l :: Float
    l = fromIntegral (length xs)
--instance Show Polynomial where
  --show xs = foldr f [] xs
  --  where
  --    l :: Float
  --    l = fromIntegral (length xs)
  --    f x y = (GHC.Show.show x) ++ "x^" ++ GHC.Show.show l ++ "+" ++ y

instance Equation Polynomial where
  differentiate :: [Float] -> [Float]
  differentiate (x:[]) = []
  differentiate (x:xs) = ((l * x):(differentiate xs))
    where 
      l :: Float
      l = (fromIntegral . length) xs

  integrate :: [Float] -> [Float]
  integrate [] = [0]
  integrate (x:xs) = ((x/(l+1)):(integrate xs))
    where 
      l :: Float
      l = (fromIntegral . length) xs

-- * 10 ^ p
--e :: Float -> Float -> Float
--e x p = x * (10 ** p)