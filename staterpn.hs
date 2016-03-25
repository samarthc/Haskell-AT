{- A reverse Polish notation calculator with command line argument support. Returns a Nothing if the expression is invalid.
   Current list of functions handled:
     +, -, *, /
-}

import Control.Monad.State
import Data.List (words)
import System.Environment

readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of
    [(x,"")] -> Just x
    _ -> Nothing

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

pushMaybe :: Maybe a -> State (Maybe [a]) ()
pushMaybe Nothing = put Nothing
pushMaybe (Just x) = push x

build :: [String] -> State (Maybe [Double]) () --Builds the stack step by step, evaluating and pushing the result as it goes.
build [] = return ()
build (x:xs) = do
    case x of
        "+" -> eval (+)
        "-" -> eval (-)
        "*" -> eval (*)
        "/" -> eval (/)
        _ -> pushMaybe (readMaybe x) --If number, readMaybe returns (Just n), which pushMaybe pushes onto the stack. Otherwise, pushMaybe puts Nothing as the state and the computation fails.
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

main = do
    args <- getArgs
    case args of
        [] -> (fmap (show . solve) getLine) >>= putStrLn
        _ -> mapM_ (putStrLn . show . solve) args