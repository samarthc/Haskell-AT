import Control.Monad.State
import Data.List (words)

readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of
    [(x, "")] -> Just x
    _ -> Nothing

eval :: String -> State [String] (Maybe Double)
eval op = do
    a <- liftM join . (liftM . liftM $ readMaybe) $ pop --Hacky code
    b <- liftM join . (liftM . liftM $ readMaybe) $ pop --Hacky code
    case op of 
        "+" -> return (liftM2 (+) b a)
        "-" -> return (liftM2 (-) b a)
        "*" -> return (liftM2 (*) b a)
        "/" -> return (liftM2 (/) b a)
        _ -> return Nothing

pop :: State [a] (Maybe a)
pop = state safePop
    where
        safePop [] = (Nothing, [])
        safePop (x:xs) = (Just x, xs)

push :: a -> State [a] ()
push x = state $ \xs -> ((), x:xs)

reduce :: State [String] ()
reduce = do
    cur <- get
    if(length cur < 3) then
        return ()
    else do
        Just op <- pop
        res <- eval op
        case res of
            Just val -> push (show val)
            Nothing -> fail "Expression invalid" --Because I'm hacky/Clap along, if you feel, like a coder with no remorse
    --Seriously though, failure is handled with Nothing everywhere else. Do something similar in this situation.
        reduce

solve :: String -> Maybe Double
solve exp = report (runState reduce (reverse . words $ exp))
    where
        report ((), [x]) = readMaybe x
        report _ = Nothing