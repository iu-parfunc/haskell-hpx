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

module Foreign.HPX {-
(
  Action(..),
  Foreign.HPX.init, initWith,
  registerAction,
  run, shutdown
)
       -}  where

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C2HS

import System.Environment          (getProgName, getArgs)
import Control.Monad               (liftM, liftM2)
import Data.Map            as M
import Data.IORef
import System.IO.Unsafe            (unsafePerformIO)

#include "hpx/hpx.h"

--------------------------------------------------------------------------------
-- Global Mutable Structures
--------------------------------------------------------------------------------

{-# NOINLINE hpxActionTable #-}
hpxActionTable :: IORef (M.Map (FunPtr a) Action)
hpxActionTable = unsafePerformIO$ newIORef M.empty

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
init = initWith =<< (liftM2 (:) getProgName $ getArgs)

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

foreign import ccall "dynamic"
  unwrap :: FunPtr (Ptr () -> IO CInt) -> (Ptr () -> IO CInt)

--Different # of args:

-- {# fun unsafe hpx_register_action as ^
--  { alloca-      `Action' peekAction*
--  , withCString* `String'
--  , id           `Handler'
--  } -> `Int' cIntConv #}
--  where peekAction = liftM Action . peek

hpxRegisterAction = undefined

{-# INLINEABLE registerAction#-}
registerAction :: String -> (Ptr () -> IO CInt) -> IO ()
registerAction s p = do
  ptr <- wrap p
  (r, act) <- hpxRegisterAction s ptr
  -- TODO throw exception on 'r'
  modifyIORef hpxActionTable (M.insert ptr act)
  putStrLn$ "Registered action pointer: "++ show ptr ++ " with key: "++ show s

--------------------------------------------------------------------------------
-- Runtime Calls
--------------------------------------------------------------------------------

-- hpx_run must have been renamed

-- {# fun hpx_run as ^
--  { withAction*  `Action'
--  , id           `Ptr ()'
--  , cIntConv     `Int'
--  } -> `Int' cIntConv #}
--  where withAction = with . useAction

hpxRun = undefined

run :: (Ptr () -> IO CInt) -> Ptr () -> Int -> IO Int
run p args size = do
  tbl <- readIORef hpxActionTable
  ptr <- wrap p
  case M.lookup ptr tbl of
     Nothing -> error$ "ERROR: Invalid action pointer: "++ show ptr
     Just action -> hpxRun action args size

{# fun hpx_shutdown as ^ { cIntConv `Int' } -> `()' #}

shutdown :: Int -> IO ()
shutdown = hpxShutdown
