{-|
Module:      Foreign.HPX
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

peekIntConv :: (Storable a, Integral a, Integral b)
            => Ptr a -> IO b
peekIntConv = fmap fromIntegral . peek

withIntConv :: (Storable b, Integral a, Integral b)
            => a -> (Ptr b -> IO c) -> IO c
withIntConv = with . fromIntegral
