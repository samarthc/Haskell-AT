module SchemeEval where

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp (*)),
              ("mod", numericBinOp mod),
              ("quotient", numericBinOp quot),
              ("remainder", numericBinOp rem)]

numericBinOp :: Num a => (a -> a -> a) -> [LispVal] -> LispVal
numericBinOp op params = let x = foldl1 op $ map unpackNum params in case x of
    (_ % _) -> Ratio x
    (_ :+ _) -> Complex x
    otherwise -> if isInt x then Number x else Float x
    where isInt x = x == (round x)

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
