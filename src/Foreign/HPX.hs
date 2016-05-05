{-# LANGUAGE RankNTypes #-}

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
import           Control.Exception (assert)
import           Control.Monad (mapAndUnzipM, replicateM, unless, void)
import           Control.Monad.IO.Class (MonadIO(..))

import           Data.Binary (Binary(..), decode, encode)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Unsafe
import           Data.Foldable (for_)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Traversable (for)

import           Foreign hiding (void)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.HPX.Types

import           GHC.StaticPtr

import           Prelude hiding (init)

import           System.Environment (getArgs, getProgName)
import           System.Exit (exitFailure)

-- Smart constructor for ActionSpec
actionSpec :: Binary a => StaticPtr (a -> HPX r) -> ActionSpec
actionSpec = ActionSpec Default

withHPX :: [ActionSpec] -> HPX () -> IO ()
withHPX specs = withHPXs specs . (:[])

withHPXs :: [ActionSpec] -> [HPX ()] -> IO ()
withHPXs specs fs = do
  action_ptrs <- replicateM (length specs) $ new Invalid
  tramp_ptr   <- registerTrampoline
  let action_ptrs_and_specs = zip action_ptrs specs
      action_env            = actionSpecsToEnv action_ptrs_and_specs
      main_actions          = map (runHPX action_env) fs
  action_fun_ptrs            <- for action_ptrs_and_specs $
                                    uncurry (registerAction action_env)
  (main_ptrs, main_fun_ptrs) <- mapAndUnzipM registerMain main_actions
  _ <- init
  for_ main_ptrs $ runTrampoline tramp_ptr
  c'hpx_finalize
  free tramp_ptr
  for_ main_ptrs       free
  for_ main_fun_ptrs   freeHaskellFunPtr
  for_ action_ptrs     free
  for_ action_fun_ptrs freeHaskellFunPtr

actionSpecsToEnv :: [(Ptr Action, ActionSpec)] -> ActionEnv
actionSpecsToEnv = ActionEnv
                 . Map.fromList
                 . map (\(p, (ActionSpec _ sptr)) -> (staticKey sptr, p))

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
    p_action  <- new Invalid
    -- TODO: Figure out what error codes can be produced here
    void $ withCString "haskell_hpx_trampoline:keyName" $ \c_keyName ->
        c'hpx_register_action_1 C'HPX_DEFAULT
                                NoAttribute
                                c_keyName
                                (castPtr p_action)
                                2
                                (castFunPtr p'haskell_hpx_trampoline)
                                c'HPX_ACTION_T
    pure p_action

-- Unsafe
registerMain :: IO () -> IO (Ptr Action, FunPtr (Ptr () -> CSize -> IO CInt))
registerMain mainAction = do
    p_action  <- new Invalid
    c_clbk    <- mk'hpx_action_handler_t clbk
    -- TODO: Figure out what error codes can be produced here
    void $ withCString "haskell_hpx_main:keyName" $ \c_keyName ->
        c'hpx_register_action_2 C'HPX_DEFAULT
                                Marshalled
                                c_keyName
                                (castPtr p_action)
                                3
                                (castFunPtr c_clbk)
                                c'HPX_POINTER
                                c'HPX_SIZE_T
    pure (p_action, c_clbk)
  where
    clbk :: Ptr () -> CSize -> IO CInt
    clbk _ _ = 0 <$ mainAction

-- Unsafe
runTrampoline :: Ptr Action -> Ptr Action -> IO ()
runTrampoline trampPtr mainPtr = void $
    c'_hpx_run_1 (castPtr trampPtr) 1 (castPtr mainPtr)

-- Unsafe
registerAction :: ActionEnv -> Ptr Action -> ActionSpec
               -> IO (FunPtr (Ptr () -> CSize -> IO CInt))
registerAction env pAction (ActionSpec actionT sptr) = do
    c_clbk <- mk'hpx_action_handler_t clbk
    -- TODO: Figure out what error codes can be produced here
    void $ withCString keyName $ \c_keyName ->
        c'hpx_register_action_2 (toC actionT)
                                Marshalled
                                c_keyName
                                (castPtr pAction)
                                3
                                (castFunPtr c_clbk)
                                c'HPX_POINTER
                                c'HPX_SIZE_T
    pure c_clbk
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
    clbk cstr _ = do
        arg <- decodeValue cstr
        _   <- runHPX env $ deRefStaticPtr sptr arg
        pure 0

-- See Note [Encoding values]
withEncodedValue :: Binary a => a -> (CString -> CSize -> IO b) -> IO b
withEncodedValue val f =
    let bsEncVal    = BL.toStrict $ encode val
        bsEncLen    = BL.toStrict $ encode $ BS.length bsEncVal
        bsEncLenVal = bsEncLen <> bsEncVal
    in unsafeUseAsCStringLen bsEncLenVal $ \(c_arg, c_argLen) ->
        f c_arg (fromIntegral c_argLen)

-- See Note [Encoding values]
decodeValue :: Binary r => Ptr () -> IO r
decodeValue buffer = do
    lenBS <- unsafePackCStringLen (castPtr buffer, intSize)
    let len    = decode $ BL.fromStrict lenBS
        valLen = intSize + len
    valBS <- unsafePackCStringLen (castPtr buffer, valLen)
    let (len2, valPayload) = decode $ BL.fromStrict valBS
    assert (len == len2) $ pure valPayload

intSize :: Int
intSize = sizeOf (undefined :: Int)

bufferSize :: Int32
bufferSize = maxBound

call :: Binary a
     => Address -> a -> StaticPtr (a -> HPX (Promise r)) -> HPX (LCO r)
call (Address addr) arg sptr = do
    p_action <- lookupAction sptr
    liftIO $ do
        Action action <- peek p_action
        lco@(LCO result) <- newLCOFuture
        -- TODO: Exit codes
        _ <- withEncodedValue arg $ c'_hpx_call addr action result 2
        pure lco

callCC :: Binary a => Address -> a -> StaticPtr (a -> HPX ()) -> HPX ()
callCC (Address addr) arg sptr = do
    p_action <- lookupAction sptr
    liftIO $ do
        Action action <- peek p_action
        -- TODO: Exit codes
        void . withEncodedValue arg $ c'_hpx_call_cc addr action 2

callSync :: (Binary a, Binary r)
         => Address -> a -> StaticPtr (a -> HPX (Promise r)) -> HPX r
callSync (Address addr) arg sptr = do
    p_action <- lookupAction sptr
    liftIO $ allocaBytes (fromIntegral bufferSize) $ \p_buffer ->
             withEncodedValue arg $ \c_arg c_argLen -> do
                Action action <- peek p_action
                _  <- c'_hpx_call_sync addr action
                                       (castPtr p_buffer) (fromIntegral bufferSize)
                                       2 c_arg c_argLen
                decodeValue p_buffer

-- Unsafe
newLCOFuture :: IO (LCO r)
newLCOFuture = fmap LCO . c'hpx_lco_future_new $ fromIntegral bufferSize

-- Unsafe
deleteLCO :: LCO r -> HPX ()
deleteLCO (LCO lco) = liftIO $
    c'hpx_lco_delete lco c'HPX_NULL

getLCO :: Binary r => LCO r -> HPX r
getLCO (LCO lco) = liftIO $ allocaBytes (fromIntegral bufferSize) $ \ p_buffer -> do
    -- TODO: Error codes
    _  <- c'hpx_lco_get lco (fromIntegral bufferSize) (castPtr p_buffer)
    decodeValue p_buffer

threadContinue :: Binary r => r -> HPX (Promise r)
-- TODO: Exit codes
threadContinue res = liftIO $ do
    _ <- withEncodedValue res $ c'_hpx_thread_continue 2
    pure Promise

getMyRank :: HPX Int
getMyRank = liftIO $ fmap fromIntegral c'hpx_get_my_rank

getMyThreadID :: HPX Int
getMyThreadID = liftIO $ fmap fromIntegral c'hpx_get_my_thread_id

getNumRanks :: HPX Int
getNumRanks = liftIO $ fmap fromIntegral c'hpx_get_num_ranks

getNumThreads :: HPX Int
getNumThreads = liftIO $ fmap fromIntegral c'hpx_get_num_threads

isActive :: HPX Bool
isActive = liftIO $ fmap (toEnum . fromIntegral) c'hpx_is_active

timeElapsedMS :: Time -> HPX Double
timeElapsedMS t = liftIO $ with t $ \p_t -> do
    e <- c'wr_hpx_time_elapsed_ms (castPtr p_t)
    pure $ realToFrac e

timeNow :: HPX Time
timeNow = liftIO $ alloca $ \p_time -> do
    c'wr_hpx_time_now (castPtr p_time)
    peek p_time

lookupAction :: StaticPtr (a -> HPX r) -> HPX (Ptr Action)
lookupAction sp = do
  mbAction <- asksHPX $ Map.lookup (staticKey sp) . unActionEnv
  maybe (error "Lookup of unregistered action") pure mbAction

foreign import ccall "&haskell_hpx_trampoline" p'haskell_hpx_trampoline
  :: FunPtr (C'hpx_action_t -> IO CInt)

{-
Note [Encoding values]
~~~~~~~~~~~~~~~~~~~~~~

TODO: How we do it
-}
