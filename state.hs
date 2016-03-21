import Control.Monad.State

type Stack = [Int]

pop :: State Stack Int
pop [] = fail "Stack is empty"
pop = State $ \xs -> (head xs, tail xs)

push :: Int -> State Stack ()
push x = State $ \xs -> ((), x:xs)
