{-# LANGUAGE RankNTypes #-}

{-|
Module:      Foreign.Utils
Copyright:   (C) 2014-2015 Ryan Newton
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Newton
Stability:   Experimental
Portability: GHC (StaticPointers)

Miscellaneous C2HS-related utilities.
-}
module Foreign.Utils where

import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

type Consumer a = forall b. (a -> IO b) -> IO b

peekIntConv :: (Storable a, Integral a, Integral b)
            => Ptr a -> IO b
peekIntConv = fmap fromIntegral . peek

-- TODO: Figure out best resource-freeing strategy for HPX
withFunWrapper :: (a -> IO (FunPtr a)) -> a -> (FunPtr a -> IO b) -> IO b
withFunWrapper wrapper hFun f = wrapper hFun >>= f

withIntConv :: (Storable b, Integral a, Integral b)
            => a -> (Ptr b -> IO c) -> IO c
withIntConv = with . fromIntegral
