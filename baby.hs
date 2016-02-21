doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

removeNonUppercase :: String -> String
removeNonUppercase str = [x | x<-str, x `elem` ['A'..'Z']]

circumference :: Float -> Float
circumference r = 2*pi*r

circumference' :: Double -> Double
circumference' r = 2*pi*r

circum r = 2*pi*r

factorial :: Integer -> Integer
factorial n = product [1..n]

fac :: Int -> Int
fac n = product [1..n]

head' :: [a] -> a
head' [] = error "head call on empty list doesn't make sense"
head' (x:_) = x

cycle' 1 xs = xs
cycle' a xs = xs ++ (cycle' (a-1) xs)