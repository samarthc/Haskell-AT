import Data.List (genericDrop)

nextFib :: (Integral t) => (t,t) -> (t,t)
nextFib (x,y) = (x+y,x)

fibPair :: (Integral t, Integral a) => t -> (a,a)
fibPair 1 = (1,1)
fibPair n = nextFib . fibPair $ (n-1)

fib :: (Integral a, Integral b) => a -> b
fib n = let (_,x) = fibPair n in x

hackyFib :: (Integral a, Integral b) => a -> b
hackyFib n = snd . head . genericDrop (n-1) . iterate nextFib $ (1,1)