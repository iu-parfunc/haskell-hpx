{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TypeFamilies #-}

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

import           Control.Monad (liftM2, unless)

import           Data.Bifunctor (first)
import           Data.Binary (Binary(..), decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Unsafe (unsafePackCStringLen, unsafeUseAsCStringLen)
-- import qualified Data.HVect as HVect (length)
-- import           Data.HVect (HVect, sNatToInt)
import           Data.Function ((&))
import           Data.Ix (Ix)
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.IORef
import           Data.Proxy

import           Foreign
import           Foreign.C
import           Foreign.C2HS
-- import           Foreign.HPX.Classes
-- import           Foreign.LibFFI

import           GHC.StaticPtr

import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.IO.Unsafe (unsafePerformIO)

import Debug.Trace (traceShowM)

#include <hpx/hpx.h>
#include "wr_hpx_types.h"

--------------------------------------------------------------------------------
-- Global Mutable Structures
--------------------------------------------------------------------------------

-- {-# NOINLINE hpxActionTable #-}
-- hpxActionTable :: IORef (Map (FunPtr a) Action)
-- hpxActionTable = unsafePerformIO $ newIORef M.empty

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

type Consumer a = forall b. (a -> IO b) -> IO b

newtype Action a = Action { useAction :: Ptr CAction }
  deriving (Eq, Show)
type CAction = {#type hpx_action_t #}

type ActionHandler = Ptr () -> {# type size_t #} -> IO CInt

foreign import ccall unsafe "wrapper"
    newActionHandler :: ActionHandler -> IO (FunPtr ActionHandler)

-- TODO: Figure out best resource-freeing strategy for HPX
withFunWrapper :: (a -> IO (FunPtr a)) -> a -> (FunPtr a -> IO b) -> IO b
withFunWrapper wrapper hFun f = wrapper hFun >>= f

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

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
  }           -> `Int' cIntConv #}

--------------------------------------------------------------------------------
-- Action Registration
--------------------------------------------------------------------------------

--Different # of args:

-- {#fun unsafe variadic hpx_register_action[??] as ^
--   { alloca-      `Action' peekAction*
--   , withCString* `String'
--   , id           `Handler'
--   }           -> `Int' cIntConv #}
--  where peekAction = fmap Action . peek

{#enum hpx_action_type_t as ActionType
  { HPX_DEFAULT   as Default
  , HPX_TASK      as Task
  , HPX_INTERRUPT as Interrupt
  , HPX_FUNCTION  as Function
  } deriving (Bounded, Eq, Ix, Ord, Read, Show) #}

{#enum define HPXAttribute
  { HPX_ATTR_NONE  as NoAttribute
  , HPX_MARSHALLED as Marshalled
  , HPX_PINNED     as Pinned
  } deriving (Bounded, Eq, Ix, Ord, Read, Show) #}

-- -- TODO: Convert Int into Enum
-- {#fun unsafe variadic hpx_register_action[??] as ^
--   {
--   , cIntConv     `Int'
--   , withCString* `String'
--   , alloca-      `Action' peekAction*
--   , id           `Handler'
--   }           -> `Int' cIntConv #}
--  where peekAction = fmap Action . peek

-- foreign import ccall "hpx.h &hpx_register_action"
--     hpx_register_action_fun :: FunPtr ()
--
-- foreign import ccall "hpx.h &_hpx_run"
--     hpx_run_fun :: FunPtr ()
--
-- hpx_register_action :: forall f.
--                     ( All FFIType (HMap Proxy (ArgList f))
--                     , FFIFunction f
--                     , FunReturnType f ~ CInt
--                     , HVectSing (HMap Proxy (ArgList f))
--                     ) => ActionType -> Int -> String -> f -> IO (CInt, Action f)
-- hpx_register_action actionType attr key actionHandler = alloca $ \(idPtr :: Ptr CAction) -> do
--   let hArgs   = hSing :: HVect (HMap Proxy (ArgList f))
--       numArgs = fromIntegral . sNatToInt $ HVect.length hArgs
--   h  <- wrapFunPtr actionHandler
--   rc <- callFFI hpx_register_action_fun retCInt $
--     [ toArg (fromIntegral $ fromEnum actionType :: {#type hpx_action_type_t #})
--     , argCUInt (fromIntegral attr)
--     , argString key
--     , argPtr idPtr
--     , argFunPtr h
--     , argCInt numArgs
--     ] ++ map argPtr (toFFITypeList hArgs)
-- --   actionID <- fmap Action $ peek idPtr
--   return (rc, Action idPtr)
--
-- registerAction :: forall f.
--                 ( All FFIType (HMap Proxy (ArgList f))
--                 , FFIFunction f
--                 , FunReturnType f ~ CInt
--                 , HVectSing (HMap Proxy (ArgList f))
--                 ) => ActionType -> Int -> String -> f -> IO (Action f)
-- registerAction actionType attr key = fmap snd . hpx_register_action actionType attr key

{#fun pure wr_hpx_pointer as hpxPointer {} -> `Ptr ()' #}
{#fun pure wr_hpx_size    as hpxSize    {} -> `Ptr ()' #}

{#fun unsafe variadic hpx_register_action[hpx_type_t, hpx_type_t] as ^
  {              `ActionType'
  ,              `HPXAttribute'
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
               -> HPXAttribute
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

-- {-# INLINEABLE registerAction#-}
-- registerAction :: String -> (Ptr () -> IO CInt) -> IO ()
-- registerAction s p = do
--   ptr <- wrap p
--   (_r, act) <- hpxRegisterAction s ptr
--   -- TODO throw exception on '_r'
--   modifyIORef hpxActionTable (M.insert ptr act)
--   putStrLn $ "Registered action pointer: " ++ show ptr ++ " with key: " ++ show s

--------------------------------------------------------------------------------
-- Runtime Calls
--------------------------------------------------------------------------------

-- {#fun _hpx_run as ^
--   { withAction*  `Action'
--   , cIntConv     `Int'
-- --  , id           `Ptr ()'
--   }           -> `Int' cIntConv #}
--   where
--     withAction = with . useAction

-- run :: (Ptr () -> IO CInt) -> Ptr () -> Int -> IO Int
-- run p _args size = do
--   tbl <- readIORef hpxActionTable
--   ptr <- wrap p
--   case M.lookup ptr tbl of
--      Nothing -> error $ "ERROR: Invalid action pointer: " ++ show ptr
--      Just action -> hpxRun action size -- _args

{#fun variadic _hpx_run[const char*, const int] as ^
  {           useAction `Action a' Action
  ,                two- `Int'
  , withCStringLenConv* `BS.ByteString'&
  }                  -> `Int' #}
  where
    withCStringLenConv :: BS.ByteString -> Consumer (CString, CInt)
    withCStringLenConv bs f = unsafeUseAsCStringLen bs $ \(cstr, len) -> f (cstr, fromIntegral len)

run :: Binary a => Action a -> a -> IO (Int, Action a)
run action = fmap (first fromIntegral) . hpxRun action . BL.toStrict . encode

-- hpx_run :: ( ToHVect args
--            , All ToArg (HVectOf args)
--            , ArgList f ~ HVectOf args
--            ) => Action f -> args -> IO CInt
-- hpx_run action args =
--     let hArgs   = hFromTuple args
--         numArgs = fromIntegral . sNatToInt $ HVect.length hArgs
--     in callFFI hpx_run_fun retCInt $ [argPtr (useAction action), argCInt numArgs] ++ toArgList hArgs
--
-- run :: ( ToHVect args
--        , All ToArg (HVectOf args)
--        , ArgList f ~ HVectOf args
--        ) => Action f -> args -> IO ()
-- run action = void . hpx_run action

{#fun unsafe hpx_print_help as ^ {} -> `()' #}

printHelp :: IO ()
printHelp = hpxPrintHelp

{#fun unsafe hpx_finalize as ^ {} -> `()' #}

finalize :: IO ()
finalize = hpxFinalize

{#fun hpx_exit as ^
  { cIntConv `Int'
  }       -> `()' #}

exit :: Int -> IO ()
exit = hpxExit

two :: Num n => Consumer n
two = (2 &)
