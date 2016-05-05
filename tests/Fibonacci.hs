{-# LANGUAGE StaticPointers #-}
module Fibonacci (actions, main) where

import Control.Monad.IO.Class (MonadIO(..))
import Foreign.HPX
import GHC.StaticPtr
import System.IO (hFlush, stdout)
import Text.Printf

fibOf :: Int
fibOf = 5

fibAction :: Int -> HPX (Promise Int)
fibAction = threadContinue . (+1)
-- fibAction n
--   | n < 2     = threadContinue n
--   | otherwise = do
--         liftIO $ putStrLn "wat"
--         fut1 <- call Here (n - 1) fibActionSP
--         fut2 <- call Here (n - 2) fibActionSP
--         fn1 <- getLCO fut1
--         fn2 <- getLCO fut2
--         liftIO $ do
--             putStrLn "-------"
--             print n
--             print fn1
--             print fn2
--             putStrLn "-------"
--         deleteLCO fut1
--         deleteLCO fut2
--         threadContinue (fn1 + fn2)

fibActionSP :: StaticPtr (Int -> HPX (Promise Int))
fibActionSP = static fibAction

actions :: [ActionSpec]
actions = [actionSpec fibActionSP]

main :: HPX ()
main = do
    liftIO $ do
        printf "fib(%d) = " fibOf
        hFlush stdout
--     before <- timeNow

    fn <- callSync Here fibOf fibActionSP

--     elapsedMS <- timeElapsedMS before
--     let elapsed = elapsedMS / 1e3
--     localities <- getNumRanks
--     threads    <- getNumThreads
    liftIO $ do
          printf "%d\n"                   fn
--           printf "seconds: %.7f\n"        elapsed
--           printf "localities: %d\n"       localities
--           printf "threads/locality: %d\n" threads
