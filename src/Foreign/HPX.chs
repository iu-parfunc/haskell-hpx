{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}

{-|
Module:      Foreign.HPX
Copyright:   (C) 2014-2015 Ryan Newton
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Newton
Stability:   Experimental
Portability: GHC (StaticPointers)

Haskell bindings for HPX.
-}
module Foreign.HPX where

import           Control.Monad (liftM2, unless, zipWithM)

import           Data.Binary (Binary(..), decode, encode)
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Unsafe (unsafePackCStringLen)
import           Data.Function ((&))
import           Data.Functor (void)
import           Data.IORef
import           Data.Ix (Ix)
import           Data.List (genericLength)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Typeable

import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Utils

import           GHC.StaticPtr

import           Prelude hiding (init)

import           System.Clock (TimeSpec(..))
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.IO.Unsafe (unsafePerformIO)

#include <hpx/hpx.h>
#include "wr_hpx.h"

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- newtype Action a r = Action { useAction :: Ptr HPXAction }
--   deriving (Eq, Ord, Storable)
type HPXAction = {#type hpx_action_t #}

type HSUpcall = {#type hs_upcall_t #}

-- castAction :: (Typeable a, Typeable b, Typeable r, Typeable s) => Action a r -> Maybe (Action b s)
-- castAction = cast
--
-- withAction :: Action a r -> Consumer b HPXAction
-- withAction = withPtr . useAction

type ActionHandler = Ptr () -> Size -> IO CInt
-- type ActionHandler = Ptr () -> CULong -> IO CInt

foreign import ccall unsafe "wrapper"
    newActionHandler :: ActionHandler -> IO (FunPtr ActionHandler)

type GlobalAddress = {#type hpx_addr_t #}
data LCO a = LCO
  { globalAddress     :: GlobalAddress
  , globalAddressSize :: Word
  } deriving (Eq, Ord, Read, Show)

withLCOArray :: (Num n1, Num n2, Storable n2) => [LCO a] -> (n1 -> Ptr GlobalAddress -> Ptr n2 -> IO b) -> IO b
withLCOArray lcos f =
  let !n     = genericLength lcos
      !addrs = map globalAddress                      lcos
      !sizes = map (fromIntegral . globalAddressSize) lcos
  in withArray addrs $ \addrArray ->
     withArray sizes $ \sizeArray ->
     f n addrArray sizeArray

castLCO :: (Typeable a, Typeable b) => LCO a -> Maybe (LCO b)
castLCO = cast

{#enum hpx_action_type_t as ActionType
  { HPX_DEFAULT   as Default
  , HPX_TASK      as Task
  , HPX_INTERRUPT as Interrupt
  , HPX_FUNCTION  as Function
  } deriving (Bounded, Eq, Ix, Ord, Read, Show) #}

{#enum define Attribute
  { HPX_ATTR_NONE  as NoAttribute
  , HPX_MARSHALLED as Marshalled
  , HPX_PINNED     as Pinned
  , HPX_INTERNAL   as Internal
  } deriving (Bounded, Eq, Ix, Ord, Read, Show) #}

type HPXStatus = {#type hpx_status_t #}
newtype Status = Status HPXStatus
  deriving (Bounded, Bits, Enum, Eq, FiniteBits, Integral, Num, Ord, Read, Real, Show, Storable)

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

withHPX :: ([String] -> IO a) -> IO a
withHPX f = do
  args <- init
  res  <- f args
  finalize
  pure res

{-# INLINEABLE init #-}
init :: IO [String]
init = initWith =<< liftM2 (:) getProgName getArgs

initWith :: [String] -> IO [String]
initWith argv =
  withMany withCString argv $ \argv'  -> -- argv' :: [CString]
  withArray argv'           $ \argv'' -> -- argv'' :: Ptr CString
  with argv''               $ \argv''' -> do -- argv''' :: Ptr (Ptr CString)

    (r, argc) <- hpxInit (length argv) argv'''
    unless (r == 0) $ printHelp >> exitFailure
    x <- peekArray argc =<< peek argv''' -- :: [Ptr CString]
    mapM peekCString x

{#fun unsafe hpx_init as ^
  { withIntConv* `Int' peekIntConv*
  , id           `Ptr (Ptr (Ptr CChar))'
  }           -> `Int' #}

--------------------------------------------------------------------------------
-- Addresses
--------------------------------------------------------------------------------

{#fun pure wr_hpx_null as hpxNull {} -> `GlobalAddress' id #}
{#fun pure wr_hpx_here as hpxHere {} -> `GlobalAddress' id #}

nullAddress :: GlobalAddress
nullAddress = hpxNull

nullLCO :: LCO ()
nullLCO = LCO nullAddress 0

here :: GlobalAddress
here = hpxHere

foreign import ccall unsafe "hpx.h hpx_lco_future_new"
  hpxLcoFutureNew :: CInt -> IO GlobalAddress

newLCOFuture :: Word -> IO (LCO a)
newLCOFuture size = do
  addr <- hpxLcoFutureNew $ fromIntegral size
  pure $ LCO addr size

{#fun hpx_lco_delete as ^
  { globalAddress `LCO a'
  , globalAddress `LCO ()'
  }            -> `()' #}

deleteLCO :: LCO a -> LCO () -> IO ()
deleteLCO = hpxLcoDelete

foreign import ccall safe "hpx.h hpx_lco_get_all"
  hpxLcoGetAll :: CInt -> Ptr GlobalAddress -> Ptr CInt -> Ptr (Ptr ()) -> Ptr HPXStatus -> IO CInt

getAllLCO :: Storable a => [LCO a] -> IO [(a, Status)]
getAllLCO lcos =
  withLCOArray lcos $ \n addrArray sizeArray ->
  allocaArray n $ \outArray    ->
  allocaArray n $ \statusArray -> do
    void $ hpxLcoGetAll (fromIntegral n) addrArray sizeArray outArray statusArray
    outPtrs  <- peekArray n outArray
    statuses <- peekArray n statusArray
    zipWithM (\outPtr status -> peek (castPtr outPtr) >>= \out -> pure (out, Status status))
             outPtrs statuses

--------------------------------------------------------------------------------
-- Action Registration
--------------------------------------------------------------------------------

{#fun pure wr_hpx_pointer as hpxPointer {} -> `Ptr ()' #}
{#fun pure wr_hpx_size    as hpxSize    {} -> `Ptr ()' #}

{#fun unsafe variadic hpx_register_action[hpx_type_t, hpx_type_t] as ^
  {              `ActionType'
  ,              `Attribute'
  ,              `String'
  ,      alloca- `Ptr HPXAction' id
  , withHandler* `ActionHandler'
  ,         two- `Word'
  ,    pointerT- `Ptr ()'
  ,       sizeT- `Ptr ()'
  }           -> `Int' #}
  where
    pointerT, sizeT :: Consumer a (Ptr ())
    pointerT = (hpxPointer &)
    sizeT    = (hpxSize &)

    withHandler :: ActionHandler -> (FunPtr ActionHandler -> IO a) -> IO a
    withHandler = withFunWrapper newActionHandler

registerAction :: Binary a
               => ActionType
               -> Attribute
               -> StaticPtr (a -> IO r)
               -> IO ()
registerAction actionT attr clbk = do
    -- TODO: Figure out what error codes can be produced here
    (_r, a) <- hpxRegisterAction actionT attr ptrName c_callback
    modifyIORef registeredActionTable (Map.insert key a)
  where
    key :: StaticKey
    key = staticKey clbk

    -- This is an internal name that is distinct for every static pointer,
    -- so we use that as a unique ID
    ptrName :: String
    ptrName = case staticPtrInfo clbk of
                   StaticPtrInfo { spInfoPackageKey = pk
                                 , spInfoModuleName = mn
                                 , spInfoName       = inf
                                 } -> pk ++ mn ++ inf

    c_callback :: ActionHandler
    c_callback cstr len = do
        bs <- unsafePackCStringLen (castPtr cstr, fromIntegral len)
        0 <$ deRefStaticPtr clbk (decode (BL.fromStrict bs))

registeredActionTable :: IORef (Map StaticKey (Ptr HPXAction))
registeredActionTable = unsafePerformIO $ newIORef Map.empty
{-# NOINLINE registeredActionTable #-}

--------------------------------------------------------------------------------
-- Runtime Calls
--------------------------------------------------------------------------------

lookupAction :: StaticPtr (a -> IO r) -> IO (Ptr HPXAction)
lookupAction sp = do
  mbAction <- Map.lookup (staticKey sp) <$> readIORef registeredActionTable
  maybe (error "Lookup of unregistered action") return mbAction

{#fun variadic _hpx_run[const char*, const int] as ^
  {                  id `Ptr HPXAction'
  ,                two- `Word'
  , withCStringLenConv* `BS.ByteString'&
  }                  -> `Int' #}

run :: Binary a => StaticPtr (a -> IO r) -> a -> IO Int
run sp args = do
  action <- lookupAction sp
  hpxRun action . BL.toStrict . encode $ args

{#fun variadic _hpx_call[const char*, const int] as ^
  {                  id `GlobalAddress'
  ,                  id `HPXAction'
  ,       globalAddress `LCO r'
  ,                two- `Word'
  , withCStringLenConv* `BS.ByteString'&
  }                  -> `Int' #}

call :: Binary a => GlobalAddress -> StaticPtr (a -> IO r) -> LCO r -> a -> IO Int
call execAddr sp resAddr args = do
  action <- peek =<< lookupAction sp
  hpxCall execAddr action resAddr . BL.toStrict . encode $ args

foreign import ccall safe "hpx.h _hpx_call_sync"
    hpxCallSync :: GlobalAddress -> HPXAction -> Ptr a -> Size -> CInt -> CString -> CInt -> IO CInt

callSync :: forall a r. (Binary a, Storable r) => GlobalAddress -> StaticPtr (a -> IO r) -> a -> IO r
callSync addr sp args =
  allocaLen $ \(outPtr :: Ptr r, outLen) ->
  withCStringLenConv (BL.toStrict $ encode args) $ \(argStr, argLen) -> do
    action <- peek =<< lookupAction sp
    -- TODO: Figure out what error codes can be produced here
    _res <- hpxCallSync addr action outPtr outLen 2 argStr argLen
    peek outPtr

{#fun variadic _hpx_thread_continue[const char*, const int] as ^
  {                two- `Word'
  , withCStringLenConv* `BS.ByteString'&
  }                  -> `()' #}

threadContinue :: Binary a => a -> IO a
threadContinue args = do
  hpxThreadContinue . BL.toStrict . encode $ args
  return args

{#fun unsafe hpx_print_help as ^ {} -> `()' #}

printHelp :: IO ()
printHelp = hpxPrintHelp

{#fun unsafe hpx_finalize as ^ {} -> `()' #}

finalize :: IO ()
finalize = hpxFinalize

{#fun hpx_exit as ^
  {    `Int'
  } -> `()' #}

exit :: Int -> IO ()
exit = hpxExit

two :: Num n => Consumer a n
two = (2 &)

#if defined(linux_HOST_OS)
type HPXTime = TimeSpec
#else
type HPXTime = {#type hpx_type_t #}
#endif

{#pointer *hpx_time_t as HPXTimePtr -> HPXTime #}

{#fun wr_hpx_time_now as hpxTimeNow
  { alloca- `HPXTime' peek*
  }      -> `()' #}

timeNow :: IO HPXTime
timeNow = hpxTimeNow

{#fun wr_hpx_time_elapsed_ms as hpxTimeElapsedMS
  {    with* `HPXTime'
  }       -> `Double' #}

timeElapsedMS :: HPXTime -> IO Double
timeElapsedMS = hpxTimeElapsedMS

{#fun hpx_get_num_ranks as ^ {} -> `Int' #}

localities :: IO Int
localities = hpxGetNumRanks

{#fun hpx_get_num_threads as ^ {} -> `Int' #}

threads :: IO Int
threads = hpxGetNumThreads


foreign import ccall unsafe "wr_hpx.h hs_hpx_extra_init"
  hsHPXExtraInit :: IO ()

-- Type matches `hs_upcall_t`:
foreign import ccall unsafe "wr_hpx.h set_hs_upcall"
  setHSUpcall :: HSUpcall -> IO ()
