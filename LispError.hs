module LispError where

import LispVal
import Control.Monad.Error
import Data.List (unwords)
import Text.ParserCombinators.Parsec (ParseError)

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    show (NumArgs expected found) = "Expected " ++ show expected ++ " args; found value(s): " ++ (unwords . map show) found
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ "; found " ++ show found
    show (Parser parseError) = "Parse error at " ++ show parseError
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func) = message ++ ": " ++ func
    show (UnboundVar message varname) = message ++ ": " ++ varname

instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default

trapError action = catchError action (return . show)

extractValue :: Either LispError a -> a
extractValue (Right val) = val
