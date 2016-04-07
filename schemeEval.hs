module SchemeEval where

eval :: LispVal -> LispVal
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Ratio _) = val
eval val@(Complex _) = val
eval val@(Character _) = val
eval val@(String _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
