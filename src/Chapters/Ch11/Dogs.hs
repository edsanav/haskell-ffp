module Dogs where

data PugType = PugData

data HuskyType a = HuskyData


data DogueDeBordeaux doge = DogueDeBordeaux doge


data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

---

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Manufacturer Airline deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane Mini PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars [] = []
areCars (x:xs) = isCar x:areCars(xs)

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu (Plane m _) = m