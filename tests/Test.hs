{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr

import Foreign.HPX					as HPX


foreign import ccall "wrapper"
-- wrap :: (CDouble -> CDouble) -> IO (FunPtr (CDouble -> CDouble))
  wrap :: (Ptr () -> IO CInt) -> IO (FunPtr (Ptr () -> IO CInt))

foo :: Ptr () -> IO CInt
foo x = do
  print x
  return 0

main :: IO ()
main = do
  putStrLn " [Test] Call hpx init..."
  strs <- HPX.initWith ["--hpx-loglevel=all", "foobar"]
  fooPtr <- wrap foo
  putStrLn (" [Test] Got function pointer "++ show fooPtr)

  act <- hpxRegisterAction "__hpx_foo" fooPtr

  putStrLn$  "Done with hpx init, returned "++ show (unlines strs)

