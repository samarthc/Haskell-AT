module SchemeEval where

import LispVal
import GHC.Real
import Data.Complex

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp (*)),
              ("mod", numericBinOp mod),
              ("quotient", numericBinOp quot),
              ("remainder", numericBinOp rem)]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: Num a => LispVal -> a
unpackNum (Number num) = fromIntegral num
--unpackNum (Float num) = num
--unpackNum (Ratio num) = fromRational num
--unpackNum (Complex num) = num

eval :: LispVal -> LispVal
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Ratio _) = val
eval val@(Complex _) = val
eval val@(Character _) = val
eval val@(String _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives
