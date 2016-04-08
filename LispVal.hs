module LispVal where

import Data.Complex
import Data.Ratio
import Data.Array
import Data.Foldable (Foldable(..))

data LispVal = Atom String
             | Number Integer
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Character Char
             | String String
             | Bool Bool
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (Array Int LispVal)
             | Unspecified deriving (Eq)

instance Show LispVal where
    show (Atom name) = name
    show (Number num) = show num
    show (Float num) = show num
    show (Ratio num) = show num
    show (Complex num) = show num
    show (Character ch) = "#\\" ++ [ch]
    show (String str) = "\"" ++ str ++ "\""
    show (Bool bool) = if bool then "#t" else "#f"
    show (List list) = "(" ++  (unwords . map show) list ++ ")"
    show (DottedList list val) = "(" ++ (unwords . map show) list ++ " . " ++ show val ++ ")"
    show (Vector array) = let list = foldMap (:[]) array in "#(" ++ (unwords . map show) list ++ ")"
    show Unspecified = "unspecified value"
