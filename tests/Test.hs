{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE StaticPointers #-}

import Debug.Trace (traceM)

import Foreign
import Foreign.C
import Foreign.HPX as HPX

fib :: String -> IO Int
fib x = do
  traceM x
  HPX.shutdown 0

fibMain :: String -> IO Int
fibMain x = do
  traceM x
  HPX.shutdown 0

main :: IO ()
main = do
  traceM " [Test] Call hpx init..."
  strs <- HPX.init
  traceM $  "Done with hpx init, returned " ++ show (unlines strs)

  traceM " [Test] Registering actions fib and fibMain..."
  fibAction     <- HPX.registerAction Default Marshalled "__hpx_fib" $ static fib
  fibMainAction <- HPX.registerAction Default Marshalled "__hpx_fibMain" $ static fibMain
  fib2Action    <- HPX.registerAction Default Marshalled "__hpx_fib2" $ static fib

--   traceShowM [rc1, rc2, rc3]
--   traceShowM =<< peek (useAction fibAction)
--   traceShowM =<< peek (useAction fibMainAction)
--   traceShowM =<< peek (useAction fib2Action)

  traceM " [Test] Running HPX..."
  HPX.run fibMainAction "Hello, World!"
  traceM " [Test] Exiting HPX..."
