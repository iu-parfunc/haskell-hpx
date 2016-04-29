{-|
Module:      Foreign.HPX
Copyright:   (C) 2014-2015 Ryan Newton
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Newton
Stability:   Experimental
Portability: GHC

Haskell bindings for HPX.
-}
module Foreign.HPX (
      module Foreign.HPX
    , module Foreign.HPX.Types
    ) where

import           Bindings.HPX

import           Control.Applicative (liftA2)
import           Control.Monad (unless, void)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Reader (asks)

import           Data.Binary (Binary(..), decode, encode)
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Unsafe
import           Data.Foldable (for_)
import qualified Data.Map as Map

import           Foreign hiding (void)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.HPX.Types

import           GHC.StaticPtr

import           Prelude hiding (init)

import           System.Environment (getArgs, getProgName)
import           System.Exit (exitFailure)
import           System.IO.Unsafe (unsafePerformIO)

newActionSpec :: Binary a => StaticPtr (a -> HPX ()) -> IO ActionSpec
newActionSpec sptr = do
    p_action <- new Invalid
    pure $ ActionSpec p_action Default sptr

withHPX :: [ActionSpec] -> HPX () -> IO ()
withHPX specs f = do
  tramp_ptr            <- registerTrampoline
  let action_env        = actionSpecsToEnv specs
      main_action       = runHPX action_env f
  for_ specs $ registerAction action_env
  main_ptr             <- registerMain main_action
  _ <- init
  runTrampoline tramp_ptr main_ptr
  c'hpx_finalize

actionSpecsToEnv :: [ActionSpec] -> ActionEnv
actionSpecsToEnv = Map.fromList . map (\(ActionSpec p _ sptr) -> (staticKey sptr, p))

{-# INLINEABLE init #-}
init :: IO [String]
init = initWith =<< liftA2 (:) getProgName getArgs

initWith :: [String] -> IO [String]
initWith argv =
  with (fromIntegral (length argv)) $ \p_argc -> -- p_argc      :: Ptr CInt
  withMany withCString argv         $ \argv'  -> -- argv'       :: [CString]
  withArray argv'                   $ \argv'' -> -- argv''      :: Ptr CString
  with argv''                       $ \argv''' -> do -- argv''' :: Ptr (Ptr CString)

    r <- c'hpx_init p_argc argv'''
    unless (r == 0) $ c'hpx_print_help >> exitFailure
    argc <- fromIntegral <$> peek p_argc
    x <- peekArray argc =<< peek argv''' -- :: [Ptr CString]
    mapM peekCString x

-- Unsafe
registerTrampoline :: IO (Ptr Action)
registerTrampoline = do
    c_keyName <- newCString "haskell_hpx_trampoline:keyName"
    p_action  <- new Invalid
    -- TODO: Figure out what error codes can be produced here
    _r        <- c'hpx_register_action_1 C'HPX_DEFAULT
                                         NoAttribute
                                         c_keyName
                                         (castPtr p_action)
                                         2
                                         (castFunPtr p'haskell_hpx_trampoline)
                                         c'HPX_ACTION_T
    pure p_action

registerMain :: IO () -> IO (Ptr Action)
registerMain mainAction = do
    c_keyName <- newCString "haskell_hpx_main:keyName"
    p_action  <- new Invalid
    c_clbk    <- mk'hpx_action_handler_t clbk
    -- TODO: Figure out what error codes can be produced here
    _r        <- c'hpx_register_action_2 C'HPX_DEFAULT
                                         Marshalled
                                         c_keyName
                                         (castPtr p_action)
                                         3
                                         (castFunPtr c_clbk)
                                         c'HPX_POINTER
                                         c'HPX_SIZE_T
    pure p_action
  where
    clbk :: Ptr () -> CSize -> IO CInt
    clbk _ _ = 0 <$ mainAction

-- Unsafe
runTrampoline :: Ptr Action -> Ptr Action -> IO ()
runTrampoline trampPtr mainPtr = void $
    c'_hpx_run_1 (castPtr trampPtr) 1 (castPtr mainPtr)

-- Unsafe
registerAction :: ActionEnv -> ActionSpec -> IO ()
registerAction env (ActionSpec p_action actionT sptr) = do
    c_keyName <- newCString keyName
    c_clbk    <- mk'hpx_action_handler_t clbk
    -- TODO: Figure out what error codes can be produced here
    void $ c'hpx_register_action_2 (toC actionT)
                                   Marshalled
                                   c_keyName
                                   (castPtr p_action)
                                   3
                                   (castFunPtr c_clbk)
                                   c'HPX_POINTER
                                   c'HPX_SIZE_T
  where
    -- This is an internal name that is distinct for every static pointer,
    -- so we use that as a unique ID
    keyName :: String
    keyName = case staticPtrInfo sptr of
                   StaticPtrInfo { spInfoPackageKey = pk
                                 , spInfoModuleName = mn
                                 , spInfoName       = inf
                                 } -> pk ++ mn ++ inf

    clbk :: Ptr () -> CSize -> IO CInt
    clbk cstr len = do
        bs <- unsafePackCStringLen (castPtr cstr, fromIntegral len)
        runHPX env . deRefStaticPtr sptr . decode . BL.fromStrict $ bs
        pure 0

callCC :: Binary a => Address -> a -> StaticPtr (a -> HPX ()) -> HPX ()
callCC (Address addr) arg sptr = do
    p_action      <- lookupAction sptr
    liftIO $ do
        Action action <- peek p_action
        let bsEncArg = BL.toStrict $ encode arg
        void . unsafeUseAsCStringLen bsEncArg $ \(c_arg, c_argLen) ->
            c'_hpx_call_cc addr action 2 c_arg (fromIntegral c_argLen)

threadContinue :: Binary r => r -> HPX ()
threadContinue res = liftIO $ do
    let bsEncRes = BL.toStrict $ encode res
    void . unsafeUseAsCStringLen bsEncRes $ \(c_res, c_resLen) ->
        c'_hpx_thread_continue 2 c_res (fromIntegral c_resLen)

here :: Address
here = unsafePerformIO (Address <$> peek p'HPX_HERE)
{-# NOINLINE here #-}

-- run :: Binary a => StaticPtr (a -> IO ()) -> a -> HPX Int
-- run sptr arg = do
--   p_action <- lookupAction sptr
--   let bsEncArg = BL.toStrict $ encode arg
--   r <- unsafeUseAsCStringLen bsEncArg $ \(c_arg, c_argLen) ->
--     with c_arg                   $ \p_c_arg    ->
--     with (fromIntegral c_argLen) $ \p_c_argLen ->
--       c'_hpx_run_2 (castPtr p_action) 2 p_c_arg p_c_argLen
--   return $ fromIntegral r

lookupAction :: StaticPtr (a -> HPX ()) -> HPX (Ptr Action)
lookupAction sp = do
  mbAction <- asks $ Map.lookup (staticKey sp)
  maybe (error "Lookup of unregistered action") pure mbAction

foreign import ccall "&haskell_hpx_trampoline" p'haskell_hpx_trampoline
  :: FunPtr (C'hpx_action_t -> IO CInt)