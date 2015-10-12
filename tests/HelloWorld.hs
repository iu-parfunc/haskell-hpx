{-# LANGUAGE StaticPointers #-}

module HelloWorld (helloWorld) where

import Control.Monad

import Data.Int
import Debug.Trace

import Foreign.HPX

import GHC.StaticPtr

fib :: String -> IO ()
fib x = do
  traceM x
  exit 0

fibSP :: StaticPtr (String -> IO ())
fibSP = static fib

fibMain :: String -> IO ()
fibMain x = do
  traceM x
  -- hpx_continue ??
  exit 0

fibMainSP :: StaticPtr (String -> IO ())
fibMainSP = static fibMain

helloWorld :: IO ()
helloWorld = do
--   registerAction Default Marshalled $ static fib
  registerAction Default Marshalled fibMainSP
--   registerAction Default Marshalled $ static fib
  forever $ run fibMainSP "HELLO WORLD"


-- e :: Int
-- e = 1 + 2
--
-- x = static e
--
-- y = static e
--
-- -- instance Monad m => Monad (StaticPointer . m) where
--
-- sbind :: Monad m => m a -> StaticPtr (a -> m b) -> m b
-- sbind ma sfn =
--   undefined
--
-- -- z = return () >>= (\_ -> (return ()))
--
-- z :: IO ()
-- z = return () `sbind` static (\_ -> (return ()))
--
-- {-
--
--   static_do m1
--             b <- get r
--             m3
--             m4
--
--   $(sdo[|do m1
--             b <- get r
--             m3
--             m4])
--
--   do m1
--      hpx_call r getAction (static (\b -> m3 >> m4))
--
--   -}
--
--
-- type HpxAction a = IO a
--
-- parfib :: Int64 -> HpxAction Int64
-- parfib n | n <= 2 = return' 1
-- parfib n = do
--     xf <- spawn hpxHere (static parfib) (n-1)
--     y  <-               parfib (n-2)
--     x  <- get xf
--     return' (x+y)
--
-- -- Here's a version of return that also calls hpxContinue:
-- return' = return
-- -- Nah... we probably don't need that....
--
-- spawn = undefined
--
-- main = runHPX $ do x <- parfib 35
--                    print x
--
-- hpxHere = undefined
-- get = undefined
-- runHPX = undefined
--
-- {-
-- -- Two armed version:
-- parfib' :: Int64 -> Par Int64
-- parfib' n | n <= 2 = return' 1
-- parfib' n = do
--     xf <- spawn hpxHere (static parfib) (n-1)
--     yf <- spawn hpxHere (static parfib) (n-2)
--     x  <- get xf
--     y  <- get xf
--     return' (x+y)
-- -}
--
-- -- Cilk style:
-- --  x = spawn f
-- --  y = spawn g
-- --  sync
-- --  x + y
