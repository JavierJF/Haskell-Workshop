-- Learning DataTypes!

module Datatypes where

-- Defining a simple type with record access
data Person = Person { name :: String
                     , surname :: String
                     , age :: Int} deriving (Show, Eq)

-- This is really the way to access structures, destructuring!
age' :: Person -> Int
age' (Person _ _ a) = a

-- Recursive datatypes!

-- First a simple automatic implementation of show
data Tree' a = Leaf' | Node' a (Tree' a) (Tree' a) deriving (Show)

-- Now lets make our implementation, with show.
data Tree a = Leaf | Node a (Tree a) (Tree a)

instance (Show a) => Show (Tree a) where
  show Leaf = "Leaf"
  show (Node a Leaf Leaf) = "[Node:" ++ show a ++"]"
  show (Node a t1 Leaf) = "[Node:" ++ show a ++"]->(" ++ show t1 ++ ",Leaf)"
  show (Node a Leaf t2) = "[Node:" ++ show a ++"]->(Leaf," ++ show t2 ++ ")"
  show (Node a t1 t2) = "[Node:" ++ show a ++"]->(" ++ show t1 ++ "," ++ show t2 ++ ")"

-- Let's use this simple tree definition
-- First let's make a balacend tree, that expands creating nodes at right or
-- left depending on the value of the element in a list.
weirdTree :: Ord a => [a] -> Tree a
weirdTree [] = Leaf
weirdTree [x] = Node x Leaf Leaf
weirdTree (x:xs) = weirdTree' (Node x Leaf Leaf) xs
    where
      weirdTree' :: Ord a => Tree a -> [a] -> Tree a
      weirdTree' Leaf _ = Leaf
      weirdTree' nd [] = nd
      weirdTree' (Node x Leaf Leaf) [a] = Node x (Node a Leaf Leaf) Leaf
      weirdTree' (Node x Leaf Leaf) (y:z:xs) =
         if y < z
         then Node x (weirdTree' (Node y Leaf Leaf) xs) (Node z Leaf Leaf)
         else Node x (Node z Leaf Leaf) (weirdTree' (Node y Leaf Leaf) xs)
