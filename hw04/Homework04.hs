module Homework04 where

import Data.List



-- EXERCISE 1


{-| Reimplement each of the following functions:

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-}

fun1 :: [Integer] -> Integer
fun1 = foldl' (\x y -> x * (y - 2)) 1 . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n =
  sum $
    filter (even) $
    takeWhile (/= 1) $
      flip iterate n
        (\x -> 
          if even x then
            x `div` 2
          else
            3 * x + 1)
  

  
-- EXERCISE 2


data Tree a
  = Leaf
  | Node Int (Tree a) a (Tree a)
  deriving (Show, Eq)


foldTree :: [a] -> Tree a
foldTree =
  foldr insertIntoTree Leaf


insertIntoTree :: a -> Tree a -> Tree a
insertIntoTree x Leaf = Node 0 Leaf x Leaf
insertIntoTree x (Node height left val right)
  | treeHeight left <= treeHeight right = 
      let 
        newChild = insertIntoTree x left
      in
        Node (treeHeight newChild + 1) newChild val right
  | otherwise = 
      let 
        newChild = insertIntoTree x right
      in
        Node (treeHeight newChild + 1) left val newChild


treeHeight :: Tree a -> Int
treeHeight Leaf = -1
treeHeight (Node height _ _ _) = height



-- EXERCISE 3


-- 1. Implement a function
xor :: [Bool] -> Bool
xor = odd . (foldl (\x y -> if y then x+1 else x) 0)


-- 2. Implement map as a fold.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> ((f x) : y)) []



-- EXERCISE 4


sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  let
    end =
      2 * n + 2
    sundaram =
      map (\(x,y) -> x + y + (2 * x * y)) $
        cartProd [1..end] [1..end]
    sieve =
      filter (\x -> not (x `elem` sundaram)) [1..]
  in
   takeWhile (\x -> x <= end) $ map (\x -> 2 * x + 1) sieve


cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

