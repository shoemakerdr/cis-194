module Homework1 where

import qualified Data.List as List



-- EXERCISE 1 --


toDigits :: Integer -> [Integer]
toDigits num
  | num <= 0 = []
  | num < 10 = [num]
  | num `mod` 10 /= 0 = digits ++ [num `mod` 10]
  | otherwise = digits ++ [0]
  where digits = (toDigits $ num `div` 10)


toDigitsRev :: Integer -> [Integer]
toDigitsRev digits =
  List.reverse $ toDigits digits



-- EXERCISE 2 --


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther digits =
  List.reverse (doubleEveryOther' $ List.reverse digits)


doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [a] = [a]
doubleEveryOther' (a : b : []) = [a, b * 2]
doubleEveryOther' (a : b : rest) = [a, b * 2] ++ doubleEveryOther' rest



-- EXERCISE 3 --


sumDigits :: [Integer] -> Integer
sumDigits digits =
  List.sum $ reduceDoubleDigits digits


reduceDoubleDigits :: [Integer] -> [Integer]
reduceDoubleDigits [] = []
reduceDoubleDigits (x : xs) =
  if x < 10
    then x : (reduceDoubleDigits xs)
    else (div x 10) : (mod x 10) : (reduceDoubleDigits xs)



-- EXERCISE 4 --


validate :: Integer -> Bool
validate card =
  mod (sumDigits $ doubleEveryOther $ toDigits card) 10 == 0
