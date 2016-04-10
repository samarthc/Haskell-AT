module SchemeEnv where

import Control.Monad.Error
import Data.IORef
import LispError
import LispVal

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: Either LispError a -> ErrorT LispError IO a
liftThrows (Right a) = return a
liftThrows (Left err) = throwError err

runIOError :: ErrorT LispError IO String -> IO String
runIOError action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> ErrorT LispError IO LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Unbound variable" var) (liftIO . readIORef) (lookup var env)

setVar :: Env -> String -> LispVal -> ErrorT LispError IO LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Unbound variable" var) (liftIO . flip writeIORef value) (lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> ErrorT LispError IO LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where
    extendEnv bindings env = liftM (++env) >>= (mapM addBinding bindings)
    addBinding (var, value) = newIORef value >>= \x -> return (x, value)

