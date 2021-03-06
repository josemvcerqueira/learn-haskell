module Cipher where

import           Data.Char

myOr :: [Bool] -> Bool
myOr [] = False
myOr xs = foldr (||) False xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f xs = foldr ((||) . f) False xs

myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x ys = foldr (\v _ -> v == x) False ys

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) =
  if null $ init xs then head xs : [x] else last xs : myReverse (init (x : xs))

squish :: [[a]] -> [a]
squish []       = []
squish (x : xs) = if null x
  then squish xs
  else firstValue : squish (tail x : xs)
  where firstValue = head x

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f xs = squish . map f $ xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

makeOrdBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a
makeOrdBy o f (x : xs)
  | length xs == 1     = if f x (head xs) == o then x else head xs
  | f x (head xs) == o = makeOrdBy o f (x : tail xs)
  | otherwise          = makeOrdBy o f xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = makeOrdBy GT

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = makeOrdBy LT

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare


