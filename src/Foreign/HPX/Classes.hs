{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Foreign.HPX.Classes where

import Data.ByteString (ByteString)
import Data.HVect (HVect(..))
import Data.Int
import Data.Proxy
import Data.Word

import Foreign.C.String
import Foreign.C.Types
import Foreign.LibFFI (Arg, RetType)
import Foreign.LibFFI.FFITypes
import Foreign.LibFFI.Internal (CType)
import Foreign.LibFFI.Types
import Foreign.Ptr (FunPtr, Ptr)

import GHC.Exts (Constraint)

-------------------------------------------------------------------------------

type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
    All c '[]       = ()
    All c (x ': xs) = (c x, All c xs)

type family ArgList (a :: *) :: [*] where
    ArgList (a -> b) = a ': ArgList b
    ArgList a        = '[]

type family FunReturnType (a :: *) :: * where
    FunReturnType (a -> b) = FunReturnType b
    FunReturnType (IO a)   = a
    FunReturnType a        = a

type family HMap (f :: k1 -> k2) (xs :: [k1]) :: [k2] where
    HMap f '[]       = '[]
    HMap f (x ': xs) = f x ': HMap f xs

-------------------------------------------------------------------------------

class FFIType a where
    ffiType :: a -> Ptr CType

instance FFIType CChar      where ffiType _ = ffi_type_schar
instance FFIType CDouble    where ffiType _ = ffi_type_double
instance FFIType CFloat     where ffiType _ = ffi_type_float
instance FFIType CInt       where ffiType _ = ffi_type_sint
instance FFIType CLong      where ffiType _ = ffi_type_slong
instance FFIType CSize      where ffiType _ = ffi_type_size
instance FFIType CTime      where ffiType _ = ffi_type_size
instance FFIType CUChar     where ffiType _ = ffi_type_uchar
instance FFIType CUInt      where ffiType _ = ffi_type_uint
instance FFIType CULong     where ffiType _ = ffi_type_slong
instance FFIType CWchar     where ffiType _ = ffi_type_schar
instance FFIType (FunPtr a) where ffiType _ = ffi_type_pointer
instance FFIType Int        where ffiType _ = ffi_type_hs_int
instance FFIType Int8       where ffiType _ = ffi_type_sint8
instance FFIType Int16      where ffiType _ = ffi_type_sint16
instance FFIType Int32      where ffiType _ = ffi_type_sint32
instance FFIType Int64      where ffiType _ = ffi_type_sint64
instance FFIType (Ptr a)    where ffiType _ = ffi_type_pointer
instance FFIType Word       where ffiType _ = ffi_type_hs_word
instance FFIType Word8      where ffiType _ = ffi_type_uint8
instance FFIType Word16     where ffiType _ = ffi_type_uint16
instance FFIType Word32     where ffiType _ = ffi_type_uint32
instance FFIType Word64     where ffiType _ = ffi_type_uint64
instance FFIType a => FFIType (Proxy a) where
    ffiType Proxy = ffiType (undefined :: a)

toFFITypeList :: All FFIType xs => HVect xs -> [Ptr CType]
toFFITypeList HNil       = []
toFFITypeList (x :&: xs) = ffiType x : toFFITypeList xs

class HVectSing v where
    hSing :: HVect v

instance HVectSing '[] where
    hSing = HNil

instance HVectSing xs => HVectSing (Proxy x ': xs) where
    hSing = Proxy :&: (hSing :: HVect xs)

-------------------------------------------------------------------------------

class ToArg a where
    toArg :: a -> Arg

instance ToArg ByteString where toArg = argByteString
instance ToArg CChar      where toArg = argCChar
instance ToArg CDouble    where toArg = argCDouble
instance ToArg CFloat     where toArg = argCFloat
instance ToArg CInt       where toArg = argCInt
instance ToArg CLong      where toArg = argCLong
instance ToArg CSize      where toArg = argCSize
instance ToArg CTime      where toArg = argCTime
instance ToArg CUChar     where toArg = argCUChar
instance ToArg CUInt      where toArg = argCUInt
instance ToArg CULong     where toArg = argCULong
instance ToArg CWchar     where toArg = argCWchar
instance ToArg (FunPtr a) where toArg = argFunPtr
instance ToArg Int        where toArg = argInt
instance ToArg Int8       where toArg = argInt8
instance ToArg Int16      where toArg = argInt16
instance ToArg Int32      where toArg = argInt32
instance ToArg Int64      where toArg = argInt64
instance ToArg (Ptr a)    where toArg = argPtr
instance ToArg String     where toArg = argString
instance ToArg Word       where toArg = argWord
instance ToArg Word8      where toArg = argWord8
instance ToArg Word16     where toArg = argWord16
instance ToArg Word32     where toArg = argWord32
instance ToArg Word64     where toArg = argWord64

toArgList :: All ToArg xs => HVect xs -> [Arg]
toArgList HNil       = []
toArgList (x :&: xs) = toArg x : toArgList xs

-------------------------------------------------------------------------------

class ToHVect t where
    hFromTuple :: t -> HVect (HVectOf t)

instance {-# OVERLAPPING #-} ToHVect () where
    hFromTuple () = HNil

instance {-# OVERLAPPING #-} ToHVect (a, b) where
    hFromTuple (a, b) = a :&: b :&: HNil

instance {-# OVERLAPPABLE #-} HVectOf a ~ '[a] => ToHVect a where
    hFromTuple a = a :&: HNil

type family HVectOf (t :: *) :: [*] where
    HVectOf ()     = '[]
    HVectOf (a, b) = '[a, b]
    HVectOf a      = '[a]

-------------------------------------------------------------------------------

class HasRetType a where
    retType :: RetType a

instance HasRetType ()         where retType = retVoid
instance HasRetType ByteString where retType = retByteString
instance HasRetType CChar      where retType = retCChar
instance HasRetType CDouble    where retType = retCDouble
instance HasRetType CFloat     where retType = retCFloat
instance HasRetType CInt       where retType = retCInt
instance HasRetType CLong      where retType = retCLong
instance HasRetType CSize      where retType = retCSize
instance HasRetType CString    where retType = retCString
instance HasRetType CTime      where retType = retCTime
instance HasRetType CUChar     where retType = retCUChar
instance HasRetType CUInt      where retType = retCUInt
instance HasRetType CULong     where retType = retCULong
instance HasRetType CWchar     where retType = retCWchar
instance HasRetType Int        where retType = retInt
instance HasRetType Int8       where retType = retInt8
instance HasRetType Int16      where retType = retInt16
instance HasRetType Int32      where retType = retInt32
instance HasRetType Int64      where retType = retInt64
instance HasRetType String     where retType = retString
instance HasRetType Word       where retType = retWord
instance HasRetType Word8      where retType = retWord8
instance HasRetType Word16     where retType = retWord16
instance HasRetType Word32     where retType = retWord32
instance HasRetType Word64     where retType = retWord64

instance HasRetType a => HasRetType (FunPtr a) where
    retType = retFunPtr retType

instance HasRetType a => HasRetType (Ptr a) where
    retType = retPtr retType

-------------------------------------------------------------------------------

class ToTuple v where
    type TupleOf v :: *
    tupleFromHVect :: HVect v -> TupleOf v

instance ToTuple '[] where
    type TupleOf '[] = ()
    tupleFromHVect HNil = ()

instance ToTuple '[a] where
    type TupleOf '[a] = a
    tupleFromHVect (a :&: HNil) = a

instance ToTuple '[a, b] where
    type TupleOf '[a, b] = (a, b)
    tupleFromHVect (a :&: b :&: HNil) = (a, b)

type ArgsOf x = TupleOf (ArgList x)

-------------------------------------------------------------------------------

class FFIFunction a where
    wrapFunPtr   :: a -> IO (FunPtr a)
    unwrapFunPtr :: FunPtr a -> a

foreign import ccall "wrapper"
    wrap :: (Ptr () -> IO CInt) -> IO (FunPtr (Ptr () -> IO CInt))

foreign import ccall "dynamic"
    unwrap :: FunPtr (Ptr () -> IO CInt) -> (Ptr () -> IO CInt)

instance FFIFunction (Ptr () -> IO CInt) where
    wrapFunPtr   = wrap
    unwrapFunPtr = unwrap
