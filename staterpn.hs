import Control.Monad.State
import Data.List (words)

pop :: State (Maybe [a]) (Maybe a)
pop = state safePop
    where
        safePop (Just []) = (Nothing, Nothing)
        safePop Nothing = (Nothing, Nothing)
        safePop (Just (x:xs)) = (Just x, Just xs)

push :: a -> State (Maybe [a]) ()
push x = state safePush
    where
        safePush Nothing = ((), Nothing)
        safePush (Just xs) = ((), Just(x:xs))

build :: [String] -> State (Maybe [Double]) () --Builds the stack step by step, evaluating and pushing the result as it goes.
build [] = return ()
build (x:xs) = do
    case x of
        "+" -> eval (+)
        "-" -> eval (-)
        "*" -> eval (*)
        "/" -> eval (/)
        _ -> push (read x) --Probably a number, just push it on the stack. Note: add handling of absurd expressions
    build xs

eval :: (Double -> Double -> Double) -> State (Maybe [Double]) () --Pops twice, applies the operation and pushes the result
eval f = do
    a <- pop
    b <- pop
    case (liftM2 f b a) of
        Nothing -> put Nothing
        Just res -> push res

solve :: String -> Maybe Double
solve str = report $ runState (build (words str)) (Just [])
    where
    report (_, Just [x]) = Just x
    report _ = Nothing

main = (fmap (show . solve) getLine) >>= putStrLn