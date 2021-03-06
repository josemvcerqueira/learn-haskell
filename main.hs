module Main where

import           Data.Bool
import           Data.Char                      ( isUpper
                                                , toUpper
                                                )

myEnumFromTo :: (Ord a, Enum a) => a -> a -> [a]
myEnumFromTo x y | x > y     = []
                 | otherwise = x : myEnumFromTo z y
  where z = succ x

myWords :: String -> [String]
myWords "" = []
myWords x  = takeWhile (/= ' ') x : myWords formattedX
 where
  format     = dropWhile (== ' ') . dropWhile (/= ' ')
  formattedX = format x

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines []          = []
myLines ('\n' : xs) = myLines xs
myLines xs          = takeWhile (/= '\n') xs : myLines (dropWhile (/= '\n') xs)

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

splitByChar :: Char -> String -> [String]
splitByChar _ [] = []
splitByChar c (x : xs)
  | x == c    = splitByChar c xs
  | otherwise = takeWhile (/= c) list : splitByChar c (dropWhile (/= c) list)
  where list = x : xs

mySqr = [ x ^ 2 | x <- [1 .. 5] ]
myCube = [ y ^ 3 | y <- [1 .. 5] ]

myFilter :: String -> [String]
myFilter = filter (`notElem` ["a", "an", "the"]) . words

myZip :: [a] -> [b] -> [(a, b)]
myZip _        []       = []
myZip []       _        = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ []       _        = []
myZipWith _ _        []       = []
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys

onlyUpper :: String -> String
onlyUpper = filter isUpper

capitalize :: String -> String
capitalize []       = []
capitalize (x : xs) = toUpper x : xs

allCapital :: String -> String
allCapital []       = []
allCapital (x : xs) = toUpper x : allCapital xs

capitalFirst :: String -> Char
capitalFirst = toUpper . head

main :: IO ()
main = putStrLn "hello world"

