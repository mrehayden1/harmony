module Console.Debug (
  setDebug
, putDebug
, traceDebug
) where

import Control.Monad

import Data.IORef
import Debug.Trace

import System.IO.Unsafe (unsafePerformIO)

-- unsafePerformIO hack to get top level mutable state
debug :: IORef Bool
{-# NOINLINE debug #-}
debug = unsafePerformIO $ newIORef False

setDebug :: Bool -> IO ()
setDebug x = writeIORef debug x

putDebug :: String -> IO ()
putDebug msg = do
  en <- readIORef debug
  when en . putStrLn $ "Debug: " ++ msg

traceDebug :: String -> a -> a
{-# NOINLINE traceDebug #-}
traceDebug msg a = unsafePerformIO $ do
   en <- readIORef debug
   when en $ traceIO msg
   return a
