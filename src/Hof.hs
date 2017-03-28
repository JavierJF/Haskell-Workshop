module Hof where

fmap' :: (a -> b) -> [a] -> [b]
fmap' f [] = []
fmap' f [a] = [f a]
fmap' f (a:as) = f a : fmap' f as
-- fmap' f (a:as) = [f a] + fmap' f as

-- Weird fold in the sense of the evaluation
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ b [] = b
foldl' f b [x] = f b x
foldl' f b (x:xs) =  foldl' f (f b x) xs

-- trasverse :: (a -> b) -> [a] -> b
-- traverse = fmap f []
