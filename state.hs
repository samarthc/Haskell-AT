import Control.Monad.State

type Stack = [Int]

pop :: State Stack Int
pop = state $ safePop
    where
    safePop [] = error "Can't pop on an empty stack"
    safePop xs = (head xs, tail xs)

push :: Int -> State Stack ()
push x = state $ \xs -> ((), x:xs)

stackFind :: Int -> State Stack Int
stackFind x = do
    a <- pop
    if a == x then
        return a
    else
        stackFind x