module Database where

import           Data.Time

data DatabaseItem =  DbString String
                    | DbNumber Integer
                    | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate []                 = []
filterDbDate (DbDate time : xs) = time : filterDbDate xs
filterDbDate (_           : xs) = filterDbDate xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber []                  = []
filterDbNumber (DbNumber num : xs) = num : filterDbNumber xs
filterDbNumber (_            : xs) = filterDbNumber xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral total / fromIntegral listLength
 where
  list       = filterDbNumber xs
  total      = sum list
  listLength = length list

