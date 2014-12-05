{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.HPX     as HPX


fib :: Ptr () -> IO CInt
fib x = do
  print x
  HPX.shutdown 0
  return 0


fibMain :: Ptr () -> IO CInt
fibMain x = do
  print x
  HPX.shutdown 0
  return 0

main :: IO ()
main = do
  putStrLn " [Test] Call hpx init..."
  strs <- HPX.init
  putStrLn$  "Done with hpx init, returned "++ show (unlines strs)

  putStrLn " [Test] Registering actions fib and fibMain..."
  fib' <- HPX.registerAction "__hpx_fib" fib
  fibMain' <- HPX.registerAction "__hpx_fibMain" fibMain

  putStrLn " [Test] Running HPX..."
  HPX.run fib' nullPtr 0
  putStrLn " [Test] Exiting HPX..."
