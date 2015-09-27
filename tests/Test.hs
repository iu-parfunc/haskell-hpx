{-# LANGUAGE StaticPointers #-}
module Main (main) where

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

main :: IO ()
main = withHPX $ \_args -> do
  _fibAction    <- registerAction Default Marshalled "__hpx_fib" $ static fib
  fibMainAction <- registerAction Default Marshalled "__hpx_fibMain" $ static fibMain
  _fib2Action   <- registerAction Default Marshalled "__hpx_fib2" $ static fib

  _ <- run fibMainAction "Hello, World!"
  finalize
