{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}


module HelloWorld
  -- (helloWorld)
 where

import Control.Monad
-- import Data.Int
import Data.Typeable
import Data.ByteString.Char8
import Debug.Trace
import Foreign.HPX
import GHC.StaticPtr


_fib :: String -> IO ()
_fib x = do
  traceM x
  exit 0

_fibSP :: StaticPtr (String -> IO ())
_fibSP = static _fib

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


--------------------------------------------------------------------------------
-- Example of wrapping a StaticPtr to another with a common type.
--------------------------------------------------------------------------------

scompose :: StaticPtr (b -> c) -> StaticPtr (a -> b) -> StaticPtr (a -> c)
scompose = undefined


_wrap :: forall a b . (Read a, Show b, Typeable a, Typeable b)
     => StaticPtr (a -> b) -> StaticPtr (String -> String)
_wrap fn =
   undefined -- sh `scompose` fn `scompose` rd
  where
-- This doesn't work:
--   rd :: StaticPtr (String -> a)
--   rd = static read -- (read :: String -> a)
--   sh :: StaticPtr (b -> String)
--   sh = static show

   -- This monomorphic version works:
   t1 :: StaticPtr (StaticPtr (Int -> Int) -> (String -> String))
   t1 = static ((\ f -> show . f . read) . deRefStaticPtr)

-- This version that references the scoped type variables (dictionaries) cannot work:
--   t2 :: StaticPtr (StaticPtr (a -> b) -> (String -> String))
--   t2 = static ((\ f -> show . f . read) . deRefStaticPtr)


-- data WrappedStaticPtr where
--   WSP :: (Typeable a) => StaticPtr (a -> b) -> DynStaticPtr

data TypeRepT a

--------------------------------------------------------------------------------
-- Examples from wiki
--------------------------------------------------------------------------------

data DynStaticPtr where
  DSP :: TypeRepT a -> StaticPtr a -> DynStaticPtr

decodeStatic :: ByteString -> Maybe (DynStaticPtr, ByteString)
decodeStatic = undefined

-- UNFINISHED:
{-

data StaticApp b where
  SA :: StaticPtr (a->b) -> StaticPtr a -> StaticApp b

unStaticApp :: StaticApp a -> a
unStaticApp (SA f a) = unStatic f (unStatic a)

decodeSA :: forall b. Typeable b => ByteString -> Maybe (StaticApp b, ByteString)
decodeSA bs
  = do { (DSP (trf :: TypeRepT tfun) (fun :: StaticPtr tfun), bs1) <- decodeStatic bs
       ; (DSP (tra :: TypeRepT targ) (arg :: StaticPtr targ), bs2) <- decodeStatic bs1
            -- At this point we have
            --     Typeable b      (from caller)
            --     Typeable tfun   (from first DSP)
            --     Typeable targ   (from second DSP)
       ; Refl <- (eqT :: Int :~: Int)

       ; fun' :: StaticPtr (targ->b) <- cast ( :: tfun :~: targ -> b) fun
       ; return (SA fun' arg, bs2) }
-}


rs3 :: forall a. Typeable a => StaticPtr ([a] -> [a])
rs3 = static Prelude.reverse

-- Above works with reverse, but not this, because of show dictionary:
-- sh :: forall a. (Show a, Typeable a) => StaticPtr (a -> String)
-- sh = static Prelude.show



--------------------------------------------------------------------------------
-- Old Scraps:  Random thoughts about static bind:
--------------------------------------------------------------------------------

-- Note that the same top level varref can get two static table entries:
--
-- e :: Int
-- e = 1 + 2
--
-- x = static e
--
-- y = static e


----------------------------------------


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
