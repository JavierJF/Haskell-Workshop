module Recursion where

import Data.Maybe

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

reverse' :: [a] -> [a]
reverse' xs = reverse'' xs []
  where
    reverse'' :: [a] -> [a] -> [a]
    reverse'' [] _ = []
    reverse'' [x] xs = x:xs
    reverse'' (x:xs) ys = reverse'' xs (x:ys)

mean :: [Int] -> Double
mean [] = 0
mean [x] = fromIntegral x
mean xs = _mean xs 0 0
  where
    _mean :: [Int] -> Int -> Int -> Double
    _mean [] 0 _ = 0
    _mean [] ne c = fromIntegral c / fromIntegral ne
    _mean [x] 0 _ = 0
    _mean (x:xs) ne c = _mean xs (ne + 1) (c + x)

mean' :: [Int] -> Double
mean' =
  (\(r1,r2) -> fromIntegral r2/fromIntegral r1) . foldl (\(b1,b2) a -> (b1+1, b2+2)) (0,0)

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' xs = xs == reverse' xs
