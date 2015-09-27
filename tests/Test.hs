{-# LANGUAGE StaticPointers #-}
module Main (main) where

import Data.Functor (void)
import Debug.Trace (traceM)
import Foreign.HPX as HPX

fib :: String -> IO ()
fib x = do
  traceM x
  HPX.exit 0

fibMain :: String -> IO ()
fibMain x = do
  traceM x
  HPX.exit 0

main :: IO ()
main = do
  _ <- HPX.init

  _fibAction    <- HPX.registerAction Default Marshalled "__hpx_fib" $ static fib
  fibMainAction <- HPX.registerAction Default Marshalled "__hpx_fibMain" $ static fibMain
  _fib2Action   <- HPX.registerAction Default Marshalled "__hpx_fib2" $ static fib

  void $ HPX.run fibMainAction "Hello, World!"
