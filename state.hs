type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop [] = error "Stack is empty"
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((), Stack)
push x xs = ((), x:xs)
