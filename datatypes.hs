module Datatypes where

newtype Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir
    | CatapulsRUS
    | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
    | Plane Airline
    deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _         = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

data Person =  Person { name :: String, age :: Int } deriving (Eq, Show)
