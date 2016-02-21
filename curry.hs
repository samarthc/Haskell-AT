multThree :: (Num a) => a -> a -> a -> a

multThree x y z = x*y*z

multWithNine = multThree 9

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x = x : takeWhile' f xs
    | otherwise = []

sumOddSq = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

myCollatz :: (Integral a) => a -> [a]
myCollatz 1 = []
myCollatz n
    | odd n =  (3*n + 1) : myCollatz (3*n + 1)
    | even n = (n `div` 2) : myCollatz (n `div` 2)
myCollatz' n = n : myCollatz n

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
    | odd n = n : collatz (3*n + 1)
    | even n = n : collatz (n `div` 2)