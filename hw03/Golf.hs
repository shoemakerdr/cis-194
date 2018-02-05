module Golf where

import Data.List
import Data.Ord

-- EXERCISE 1


skips :: [a] -> [[a]]
skips l =
  takeWhile (not . null) $
    map
      (\n ->
        map fst $
          filter
            (\(_,x) -> mod x n == 0) $
            zip l [1..])
      [1..]



-- EXERCISE 2


localMaxima :: [Int] -> [Int]
localMaxima (x:y:z:rest)
  | y > x && y > z = y : localMaxima rest
  | otherwise = localMaxima rest
localMaxima _ = []



-- EXERCISE 3


histogram :: [Int] -> String
histogram [] = t
histogram xs =
  (unlines $ map (row l) [mx, (mx - 1)..1]) ++ t
  where
    l = map (\x -> (head x, length x)) $ group $ sort xs
    (_,mx) = maximumBy (comparing snd) l


t :: String
t = "\n==========\n0123456789"


row :: [(Int, Int)] -> Int -> String
row xs n =
  map (\x -> if x == True then '*' else ' ') (map (atLeast n xs) [0..9])


atLeast :: Int -> [(Int, Int)] -> Int -> Bool
atLeast n xs x =
  case find (\(m,q) -> n <= q && m == x) xs of
    Nothing -> False
    Just _  -> True


