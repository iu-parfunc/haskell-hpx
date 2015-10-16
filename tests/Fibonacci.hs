{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
module Fibonacci (fibonacci) where

import Control.Distributed.Closure
import Control.Monad
import Foreign.HPX
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import GHC.StaticPtr
import System.IO
import Text.Printf

fibAction :: Int -> IO Int
fibAction n
  | n < 2     = threadContinue n
  | otherwise = do
      lcos@[lco1, lco2] :: [LCO Int] <- replicateM 2
                                      . newLCOFuture
                                      . fromIntegral
                                      . sizeOf
                                      $ (undefined :: Int)

      _ <- call here fibActionSP lco1 (n - 1)
      _ <- call here fibActionSP lco2 (n - 2)
      [fns1, fns2] <- map fst <$> getAllLCO lcos
      deleteLCO lco1 nullLCO
      deleteLCO lco2 nullLCO

      let fn = fns1 + fns2
      threadContinue fn

tempAction :: Int -> IO Int
tempAction n = do
  printf "IN TEMP ACTION: %d\n" n
  threadContinue (888 :: Int)
  return (989 :: Int)

fibActionSP :: StaticPtr (Int -> IO Int)
-- fibActionSP = static fibAction
fibActionSP = static tempAction

fibMainAction :: Int -> IO ()
fibMainAction n = do

  printf "fib(%d)=\n" n

  now     <- timeNow
  fn      <- callSync here fibActionSP n
  elapsed <- (/ 1e3) <$> timeElapsedMS now
  l       <- localities
  t       <- threads

  printf "%d\n"                   fn
  printf "seconds: %.7f\n"        elapsed
  printf "localities: %d\n"       l
  printf "threads/locality: %d\n" t

  exit 0

fibMainActionSP :: StaticPtr (Int -> IO ())
fibMainActionSP = static fibMainAction

oneTrueUpcall :: Ptr () -> CULong -> IO CInt
oneTrueUpcall _ _ =
  do putStrLn "In oneTrueUpCall..."
     return 0

fibonacci :: IO ()
fibonacci = do

  -- TODO: Move something like this into generic run function / initialization code:
  putStrLn "[HS] Calling Extra HPX initialization"; hFlush stdout
  hsHPXExtraInit
  fptr <- newActionHandler oneTrueUpcall
  setHSUpcall fptr

  -- putStrLn "[HS] Next register fib actions (old stuff)..."
  -- registerAction Default Marshalled fibActionSP
  -- registerAction Default Marshalled fibMainActionSP
  -- putStrLn "[HS] Finally call run:"
  -- ret <- run fibMainActionSP 1
  -- putStrLn$ "[HS] HPX run exit code was "++show ret

  putStrLn$ "[HS] fibonacci call completing..."
  hFlush stdout
