{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}

--------------------------------------------------------------------------------
-- |
-- Module    : Foreign.HPX
-- Copyright : 
-- License   : BSD
--
-- Haskell Bindings for HPX.
--
--------------------------------------------------------------------------------

module Foreign.HPX (
  Action(..),
  Foreign.HPX.init, initWith,
  registerAction,
  run, shutdown
) where

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C2HS

import System.Environment  (getArgs)
import Control.Monad       (liftM)
#include "hpx/hpx.h"

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

newtype Action = Action { useAction :: {# type hpx_action_t #} }
  deriving (Eq, Show)

type Handler   = {#type hpx_action_handler_t#}


--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

{-# INLINEABLE init #-}
init :: IO [String]
init = initWith =<< getArgs

initWith :: [String] -> IO [String]
initWith argv =
  withMany withCString argv $ \argv'  -> -- argv' :: [CString]
  withArray argv'           $ \argv'' -> -- argv'' :: Ptr CString
  with argv''               $ \argv''' -> do -- argv''' :: Ptr (Ptr CString)

    (r, argc) <- hpxInit (length argv) argv'''
	-- TODO throw exception on 'r'
    x         <- peekArray argc =<< peek argv'''     -- :: [Ptr CString]
    mapM peekCString x


{# fun unsafe hpx_init as ^
  { withIntConv* `Int'                   peekIntConv*
  , id           `Ptr (Ptr (Ptr CChar))'
  } -> `Int' cIntConv #}


--------------------------------------------------------------------------------
-- Action Registration
--------------------------------------------------------------------------------

foreign import ccall "wrapper"
  wrap :: (Ptr () -> IO CInt) -> IO (FunPtr (Ptr () -> IO CInt))

{# fun unsafe hpx_register_action as ^
 { alloca-      `Action' peekAction*
 , withCString* `String'
 , id           `Handler'
 } -> `Int' cIntConv #}
 where peekAction = liftM Action . peek

{-# INLINEABLE registerAction#-}
registerAction :: String -> (Ptr () -> IO CInt) -> IO Action
registerAction s p = do
  ptr <- wrap p
  (r, act) <- hpxRegisterAction s ptr
  -- TODO throw exception on 'r'
  return act

--------------------------------------------------------------------------------
-- Runtime Calls
--------------------------------------------------------------------------------

{# fun hpx_run as ^
 { withAction*  `Action'
 , id           `Ptr ()'
 , cIntConv     `Int'
 } -> `Int' cIntConv #}
 where withAction = with . useAction

run :: Action -> Ptr () -> Int -> IO Int
run = hpxRun

{# fun hpx_shutdown as ^ { cIntConv `Int' } -> `()' #}

shutdown :: Int -> IO ()
shutdown = hpxShutdown
