-- | A place to sketch out APIs with no implementation.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}

module APISketch where

-- If we don't import this, then we don't see the extra entry in the SRT:
import StaticTest2 ()

import Data.IORef
import Data.Int
import GHC.StaticPtr
import System.IO.Unsafe

--------------------------------------------------------------------------------
-- Core types
--------------------------------------------------------------------------------

-- | QUESTION: do we need separate monads for Actions?
--   Do we actually need any State in this monad?
type Hpx a = IO a

class Compactable b where
instance Compactable Int64 where

data Future a

data HpxAddr

--------------------------------------------------------------------------------
-- Interface for high-level futures
--------------------------------------------------------------------------------

spawn :: Compactable b
      => HpxAddr -> StaticPtr (a -> Hpx b) -> a -> Hpx (Future b)

hpxHere :: HpxAddr

get :: Future a -> Hpx a

runHPX :: Hpx a -> IO a

spawn   = undefined
hpxHere = undefined
get     = undefined
runHPX  = undefined

{-# NOINLINE registeredActionTable #-}
registeredActionTable :: IORef [HpxAddr]
registeredActionTable = unsafePerformIO $ newIORef []

--------------------------------------------------------------------------------
-- Example program(s):
--------------------------------------------------------------------------------

{-
-- Two armed version:
parfib' :: Int64 -> Par Int64
parfib' n | n <= 2 = return' 1
parfib' n = do
    xf <- spawn hpxHere (static parfib) (n-1)
    yf <- spawn hpxHere (static parfib) (n-2)
    x  <- get xf
    y  <- get xf    return' (x+y)
-}

-- Cilk style:
--  x = spawn f
--  y = spawn g
--  sync
--  x + y



parfib :: Int64 -> Hpx Int64
parfib n | n <= 2 = return 1
parfib n = do
    xf :: Future Int64 <- spawn hpxHere (static parfib) (n-1)
    y  :: Int64        <-               parfib (n-2)
    x  :: Int64        <- get xf
    return (x+y)

main :: IO ()
main = do keys <- staticPtrKeys
          putStrLn$ "All keys found:\n  "++ show keys
          -- QUESTION:
          runHPX $ do x <- parfib 35
                      print x
