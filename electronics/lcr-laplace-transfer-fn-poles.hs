module Main where

import Quadratic

main :: IO ()
main = do
  let l = 1e-3
  let c = 47e-9
  let r = 291.7299829957891
  let q = (l, c, r)
  putStrLn $ show r
  putStrLn $ rootType q
  putStrLn $ (show . discriminant) q
  putStrLn $ roots q

poly :: Float -> Float -> Float -> Quadratic
poly l c r = (1, r/l, 1/l/c)

