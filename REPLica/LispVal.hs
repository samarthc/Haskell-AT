module LispVal where

import Control.Monad.Error
import Data.Complex
import Data.Ratio
import Data.Array
import Data.IORef
import Data.Foldable (Foldable(..))
import Data.List (unwords)
import System.IO
import Text.ParserCombinators.Parsec (ParseError)

type Env = IORef [(String, IORef LispVal)]

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
             | Port Handle
             | PrimitiveFunc ([LispVal] -> Either LispError LispVal)
             | IOFunc ([LispVal] -> ErrorT LispError IO LispVal)
             | Func { params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env}
             | Unspecified --deriving (Eq)

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
    show (Port handle) = "Port: " ++ show handle
    show (PrimitiveFunc _) = "<primitive>"
    show (IOFunc _) = "<IO primitive>"
    show (Func args Nothing body env) = "(lambda (" ++ unwords args ++ ") ...)"
    show (Func args (Just vararg) body env) = "(lambda (" ++ unwords args ++ " . " ++ vararg ++ ") ...)"
    show Unspecified = "unspecified value"

instance Eq LispVal where
    (Atom a1) == (Atom a2) = a1 == a2
    (Number num1) == (Number num2) = num1 == num2
    (Float num1) == (Float num2) = num1 == num2
    (Ratio num1) == (Ratio num2) = num1 == num2
    (Complex num1) == (Complex num2) = num1 == num2
    (Character c1) == (Character c2) = c1 == c2
    (String s1) == (String s2) = s1 == s2
    (Bool b1) == (Bool b2) = b1 == b2
    (List []) == (List []) = True
    (Port p1) == (Port p2) = p1 == p2
    _ == _ = False

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    show (NumArgs expected found) = "Expected " ++ show expected ++ " arg(s); found value(s): " ++ (unwords . map show) found
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ "; found " ++ show found
    show (Parser parseError) = "Parse error at " ++ show parseError
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func) = message ++ ": " ++ func
    show (UnboundVar message varname) = message ++ ": " ++ varname
    show (Default message) = message

instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default

trapError action = catchError action (return . show)

extractValue :: Either LispError a -> a
extractValue (Right val) = val
