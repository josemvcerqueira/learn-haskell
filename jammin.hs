module Jammin where

import           Data.List

data Fruit =
      Peach
    | Plum
    | Apple
    | Blackberry
    deriving (Eq, Show, Ord)

data JamJars =
    Jam { fruit :: Fruit, jars :: Int }
    deriving (Eq, Show, Ord)

row1 = Jam Peach 10
row2 = Jam Plum 12
row3 = Jam Apple 15
row4 = Jam Blackberry 90
row5 = Jam Apple 19
row6 = Jam Apple 18
row7 = Jam Apple 15
allJam = [row1, row2, row3, row4, row5, row6, row7]

myCount :: JamJars -> Int
myCount (Jam _ x) = x

mostRow :: JamJars
mostRow = foldr
  (\(Jam y x) (Jam y' x') -> if x > x' then Jam y x else Jam y' x')
  row1
  allJam

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'

allJams :: [JamJars]
allJams = sortBy compareKind allJam

equalKind :: JamJars -> JamJars -> Bool
equalKind (Jam x _) (Jam x' _) = x == x'

groupJam :: [[JamJars]]
groupJam = groupBy equalKind allJam

