module Main where

import MathExtras

main :: IO ()
main = do
  let polynomial = [4, 2, 3] :: Polynomial
  (putStrLn . showPolynomial) polynomial
  putStrLn $ showPolynomial $ integrate polynomial
