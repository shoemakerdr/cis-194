import Data.List



-- EXERCISE 1


-- fun1 :: [Integer] -> Integer
-- fun1 [] = 1
-- fun1 (x:xs)
--   | even x = (x - 2) * fun1 xs
--   | otherwise = fun1 xs


fun1 :: [Integer] -> Integer
fun1 = foldl' (\x y -> x * (y - 2)) 1 . filter even


-- fun2 :: Integer -> Integer
-- fun2 1 = 0
-- fun2 n 
--   | even n = n + fun2 (n `div` 2)
--   | otherwise = fun2 (3 * n + 1)


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



