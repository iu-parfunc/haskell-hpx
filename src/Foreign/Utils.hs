{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

{-
import qualified Data.ByteString as BS (ByteString)
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Data.Proxy

import           Foreign
import           Foreign.C

-- import           System.Posix.Signals

#include <hpx/hpx.h>

type Consumer b a = (a -> IO b) -> IO b
type Size = {#type size_t #}

allocaLen :: forall a n s. (Num n, Storable s) => Consumer a (Ptr s, n)
allocaLen g = alloca $ \ptr -> g (ptr, fromIntegral $ sizeOf (undefined :: s))

allocaLenCast :: forall a n s t. (Num n, Storable s) => Proxy s -> Consumer a (Ptr t, n)
allocaLenCast Proxy f = allocaLen g
  where
    g :: (Ptr s, n) -> IO a
    g (ptr, len) = f (castPtr ptr, len)

peekIntConv :: (Storable a, Integral a, Num b)
            => Ptr a -> IO b
peekIntConv = fmap fromIntegral . peek

peekLen :: Storable a => Ptr a -> b -> IO a
peekLen = const . peek

withCStringLenConv :: Num n => BS.ByteString -> Consumer a (CString, n)
withCStringLenConv bs f = unsafeUseAsCStringLen bs $ \(cstr, len) ->
    f (cstr, fromIntegral len)

-- TODO: Figure out best resource-freeing strategy for HPX
withFunWrapper :: (a -> IO (FunPtr a)) -> a -> (FunPtr a -> IO b) -> IO b
withFunWrapper wrapper hFun f = wrapper hFun >>= f

withIntConv :: (Storable b, Integral a, Integral b)
            => a -> (Ptr b -> IO c) -> IO c
withIntConv = with . fromIntegral

withPtr :: Storable s => Ptr s -> Consumer a s
withPtr ptr f = peek ptr >>= f

-- withSigVTALRMBlocked :: IO a -> IO a
-- withSigVTALRMBlocked action = do
--   blockSignals   $ addSignal sigVTALRM emptySignalSet
--   r <- action
--   unblockSignals $ addSignal sigVTALRM emptySignalSet
--   pure r
-}
