{-# LANGUAGE ConstraintKinds #-}
module Quadratic where
type Quadratic = (Float, Float, Float)

discriminant :: Quadratic -> Float
discriminant (a, b, c) = b*b - 4*a*c

roots :: Quadratic -> String
roots f
  | discriminant f < 0 = (show . complexRoots) f
  | otherwise          = (show . realRoots) f

realRoots :: Quadratic -> (Float, Float)
realRoots (a, b, c) = (x1, x2)
  where delta = discriminant (a, b, c)
        x1 = (-b + sqrt delta)/(2*a)
        x2 = (-b - sqrt delta)/(2*a)

complexRoots :: Quadratic -> (String, String)
complexRoots (a, b, c) = (x1, x2)
  where delta = discriminant (a, b, c)
        x1 = (show (-b / (2*a))) ++ " + " ++ "j(sqrt(" ++ (show (-delta)) ++ ")/" ++ (show (2 * a)) ++ ")" 
        x2 = (show (-b / (2*a))) ++ " - " ++ "j(sqrt(" ++ (show (-delta)) ++ ")/" ++ (show (2 * a)) ++ ")" 

--integrate :: Quadratic -> Polynomial
--integrate (a, b, c) = [a/3, b/2, c]

--differentiate :: Quadratic -> Affine
--differentiate (a, b, c) = (a, b)

rootType :: Quadratic -> String
rootType f
  | (discriminant f) <  0 = "Complex solutions"
  | (discriminant f) == 0 = "1 real solution"
  | (discriminant f) >  0 = "2 real solutions"


realRootCount :: Quadratic -> Int
realRootCount f
  | (discriminant f) <  0 = 0
  | (discriminant f) == 0 = 1
  | (discriminant f) >  0 = 2