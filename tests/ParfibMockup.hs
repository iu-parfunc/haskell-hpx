{-# LANGUAGE StaticPointers #-}

-- | Type level mockup of what a HIGH LEVEL hpx interface may look
-- like when applied to Parfib.

module ParfibMockup () where
import GHC.StaticPtr
import Control.Monad
import Debug.Trace
import Foreign.HPX
import Data.Int
----------------------------------------

type Hpx a = IO a

class Compactable b where

data Future a
data HpxAddr

spawn :: Compactable b
      => HpxAddr -> StaticPtr (a -> b) -> a -> Hpx (Future b)
spawn = undefined

hpxHere :: HpxAddr
hpxHere = undefined

get :: Future a -> Hpx a
get = undefined

runHPX :: Hpx a -> IO a
runHPX = undefined

----------------------------------------

-- | Here there would be a trampoline that calls CONTINUE only when
-- the parfib function returns.
parfib :: Int64 -> Hpx Int64
parfib n | n <= 2 = return 1
parfib n = do
    xf <- spawn hpxHere (static parfib) (n-1)
    y  <-               parfib (n-2)
    x  <- get xf
    return (x+y)

-- | Do we need to do anything special to launch the first action?
-- (And set up the trampoline?)
main = runHPX $ do x <- parfib 35
                   print x



{-
-- Two armed version:
parfib' :: Int64 -> Par Int64
parfib' n | n <= 2 = return' 1
parfib' n = do
    xf <- spawn hpxHere (static parfib) (n-1)
    yf <- spawn hpxHere (static parfib) (n-2)
    x  <- get xf
    y  <- get xf
    return' (x+y)
-}

-- Cilk style:
--  x = spawn f
--  y = spawn g
--  sync
--  x + y
