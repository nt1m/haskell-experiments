module Colors where

--import Data.ByteString hiding (take, length, drop, maximum, foldr, zip)
import Data.List
import Codec.Picture
import Data.Word
import qualified Codec.Picture.Types as M

-- Word8 = integer from 0 to 255
type Color = (Word8, Word8, Word8)
type Channel = Char

main :: IO ()
main = do
  contents <- readImage "image2.png"
  case contents of
    Left error -> putStrLn ("Could not read image: " ++ error)
    Right dynImg -> do
      let img = convertRGB8 dynImg
      putStrLn "Yay, finished converting the image"
      let pixels = convertToListOfColors img
      putStrLn "Finished processing pixels"
      let mostUsedColor = show $ getMostUsedColor pixels
      putStrLn mostUsedColor
      putStrLn "Yay, we got the most used color"
    --Right DynImage -> putStrLn ("Unexpected pixel format")

  print ""

-- Conversion between crazy library types to simple types I understand
convertToListOfColors :: Image PixelRGB8 -> [Color]
convertToListOfColors img = [fetchPixelAt img x y | x <- [0 .. (imageWidth img - 1)], y <- [0 .. (imageHeight img - 1)]] 

fetchPixelAt :: Image PixelRGB8 -> Int -> Int -> Color
fetchPixelAt img x y = (r, g, b)
  where (PixelRGB8 r g b) = pixelAt img x y

-- Sample colors list that used for testing
testColorList :: [Color]
testColorList = [(1,2,3), (1,4,6), (1,5,2), (3,4,4)]

(/>) :: Color -> Channel -> Word8
(/>) (r, _, _) 'r' = r
(/>) (_, g, _) 'g' = g
(/>) (_, _, b) 'b' = b

--(|>|) :: (Channel, Color) -> (Channel, Color) -> Bool
--(|>|) (ch1, co1) (ch2, co2) = (co1 /> ch1) > (co2 /> ch2)

--(|<|) :: (Channel, Color) -> (Channel, Color) -> Bool
--(|<|) x y = not (x |>| y)

split :: [a] -> [[a]]
split xs = [front, end]
  where
    front = take (length xs `div` 2) xs
    end   = drop (length xs `div` 2) xs

getMedian :: [Color] -> Color
getMedian [] = undefined
getMedian cs = cs !! (length cs `div` 2)

getMostUsedColor :: [Color] -> Color
getMostUsedColor xs = getMedian sortedList
  where sortedList = sortColors (getMostDiverseChannel xs) xs

isChEq :: Channel -> Color -> Color -> Bool
isChEq c c1 c2 = (c1 /> c) == (c2 /> c) 

getMostDiverseChannel :: [Color] -> Channel
getMostDiverseChannel [] = error "Median of empty list"
getMostDiverseChannel xs = (snd . maximum) [(rCount, 'r'), (gCount, 'g'), (bCount, 'b')]
  where
    rCount = getNumberOfDifferentValues 'r' xs
    gCount = getNumberOfDifferentValues 'g' xs
    bCount = getNumberOfDifferentValues 'b' xs

getNumberOfDifferentValues :: Channel -> [Color] -> Int
getNumberOfDifferentValues c xs = (length . getOccurancesOfValues (isChEq c) . sortColors c) xs

elemWithPredicate :: (a -> a -> Bool) -> a -> [a] -> Bool
elemWithPredicate p item [] = False
elemWithPredicate p item (x:xs)
  | p item x = True
  | otherwise = elemWithPredicate p item xs

-- Only works with a sorted list :/
getOccurancesOfValues :: (a -> a -> Bool) -> [a] -> [(Int, a)]
getOccurancesOfValues p xs = foldr f [] (zip (repeat 1) xs)
  where
    f (o, x) [] = [(o, x)]
    f (o, x) (y:ys)
      | p x (snd y) = ((fst y) + 1, x) : ys
      | otherwise   = (o, x) : y : ys

sortColors :: Channel -> [Color] -> [Color]
sortColors c [] = []
sortColors c (x:xs) = sortColors c lowerThanX ++ [x] ++ sortColors c moreThanX
  where
    lowerThanX = [color | color <- xs, (color /> c) <= (x /> c)]
    moreThanX = [color | color <- xs, (color /> c) > (x /> c)]    

