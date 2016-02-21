data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
data Point = Point Float Float deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r^2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x1 - x2) * abs (y1 - y2)

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Enum, Bounded)