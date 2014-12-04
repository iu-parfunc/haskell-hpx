{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.HPX     as HPX

foo :: Ptr () -> IO CInt
foo x = do
  print x
  return 0

main :: IO ()
main = do
  putStrLn " [Test] Call hpx init..."
  strs <- HPX.initWith ["Test.hs", "--hpx-loglevel=all", "foobar"]
  act <- HPX.registerAction "__hpx_foo" foo
  putStrLn$  "Done with hpx init, returned "++ show (unlines strs)

