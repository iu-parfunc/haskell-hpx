{-# LANGUAGE StaticPointers #-}
module HelloWorld (helloWorld) where

import Data.Functor (void)
import Debug.Trace (traceM)
import Foreign.HPX

fib :: String -> IO ()
fib x = do
  traceM x
  exit 0

fibMain :: String -> IO ()
fibMain x = do
  traceM x
  exit 0

helloWorld :: IO ()
helloWorld = {- withHPX $ \_args -> -} do
  _fibAction    <- registerAction Default Marshalled "__hpx_fib" $ static fib
  fibMainAction <- registerAction Default Marshalled "__hpx_fibMain" $ static fibMain
  _fib2Action   <- registerAction Default Marshalled "__hpx_fib2" $ static fib

  void $ run fibMainAction "Hello, World!"
