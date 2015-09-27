{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
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
module Foreign.HPX (
      withHPX
    , registerAction
    , run
    , finalize
    , exit
    , printHelp
    , Action(..)
    , ActionType(..)
    , Attribute(..)
    ) where

import           Control.Monad (liftM2, unless)

import           Data.Binary (Binary(..), decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Unsafe (unsafePackCStringLen, unsafeUseAsCStringLen)
import           Data.Function ((&))
import           Data.Ix (Ix)

import           Foreign
import           Foreign.C
import           Foreign.Utils

import           GHC.StaticPtr

import           Prelude hiding (init)

import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)

#include <hpx/hpx.h>
#include "wr_hpx_types.h"

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype Action a = Action { useAction :: Ptr CAction }
  deriving (Eq, Show)
type CAction = {#type hpx_action_t #}

type ActionHandler = Ptr () -> {# type size_t #} -> IO CInt

foreign import ccall unsafe "wrapper"
    newActionHandler :: ActionHandler -> IO (FunPtr ActionHandler)

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

withHPX :: ([String] -> IO ()) -> IO ()
withHPX f = f =<< init

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
  { withIntConv* `Int'                   peekIntConv*
  , id           `Ptr (Ptr (Ptr CChar))'
  }           -> `Int' #}

--------------------------------------------------------------------------------
-- Action Registration
--------------------------------------------------------------------------------

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
  } deriving (Bounded, Eq, Ix, Ord, Read, Show) #}

{#fun pure wr_hpx_pointer as hpxPointer {} -> `Ptr ()' #}
{#fun pure wr_hpx_size    as hpxSize    {} -> `Ptr ()' #}

{#fun unsafe variadic hpx_register_action[hpx_type_t, hpx_type_t] as ^
  {              `ActionType'
  ,              `Attribute'
  ,              `String'
  ,      alloca- `Action a' Action
  , withHandler* `ActionHandler'
  ,         two- `Int'
  ,    pointerT- `Ptr ()'
  ,       sizeT- `Ptr ()'
  }           -> `Int' #}
  where
    pointerT, sizeT :: Consumer (Ptr ())
    pointerT = (hpxPointer &)
    sizeT    = (hpxSize &)

    withHandler :: ActionHandler -> (FunPtr ActionHandler -> IO a) -> IO a
    withHandler = withFunWrapper newActionHandler

-- TODO: Remove the need to pass an explicit String ID
registerAction :: Binary a
               => ActionType
               -> Attribute
               -> String
               -> StaticPtr (a -> IO ())
               -> IO (Action a)
registerAction actionT attr key clbk = do
    -- TODO: Figure out what error codes can be produced here
    (_r, a) <- hpxRegisterAction actionT attr key c_callback
    return a
  where
    c_callback :: ActionHandler
    c_callback cstr len = do
        bs <- unsafePackCStringLen (castPtr cstr, fromIntegral len)
        0 <$ deRefStaticPtr clbk (decode (BL.fromStrict bs))

--------------------------------------------------------------------------------
-- Runtime Calls
--------------------------------------------------------------------------------

{#fun variadic _hpx_run[const char*, const int] as ^
  {           useAction `Action a'
  ,                two- `Int'
  , withCStringLenConv* `BS.ByteString'&
  }                  -> `Int' #}
  where
    withCStringLenConv :: BS.ByteString -> Consumer (CString, CInt)
    withCStringLenConv bs f = unsafeUseAsCStringLen bs $ \(cstr, len) -> f (cstr, fromIntegral len)

run :: Binary a => Action a -> a -> IO Int
run action = fmap fromIntegral . hpxRun action . BL.toStrict . encode

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

two :: Num n => Consumer n
two = (2 &)
