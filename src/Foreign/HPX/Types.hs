{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Foreign.HPX.Types where

import Bindings.HPX

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader (ReaderT(..))

import Data.Binary (Binary)
import Data.Data (Data)
import Data.Ix (Ix)
import Data.Map (Map)

import Foreign

import GHC.Generics (Generic, Generic1)
import GHC.Show (appPrec, appPrec1)
import GHC.StaticPtr (StaticKey, StaticPtr, staticKey, staticPtrInfo)

import Text.Printf (PrintfArg)

class C c h where
    fromC :: c -> h
    toC   :: h -> c

newtype HPX a = HPX (ReaderT ActionEnv IO a)
  deriving ( Alternative
           , Applicative
           , Functor
           , Generic
           , Generic1
           , Monad
           , MonadFix
           , MonadIO
           , MonadPlus
           , MonadReader ActionEnv
           )

-- Unsafe
runHPX :: ActionEnv -> HPX a -> IO a
runHPX env (HPX hpx) = runReaderT hpx env

type ActionEnv = Map StaticKey (Ptr Action)

newtype Action = Action C'hpx_action_t
  deriving ( Bits
           , Bounded
           , Data
           , Enum
           , Eq
           , FiniteBits
           , Generic
           , Integral
           , Ix
           , Num
           , Ord
           , PrintfArg
           , Read
           , Real
           , Show
           , Storable
           )

data ActionSpec where
    ActionSpec :: Binary a => Ptr Action -> ActionType -> StaticPtr (a -> HPX ()) -> ActionSpec

instance Eq ActionSpec where
    ActionSpec _ _ sp1 == ActionSpec _ _ sp2 = staticKey sp1 == staticKey sp2

instance Ord ActionSpec where
    compare (ActionSpec _ _ sp1) (ActionSpec _ _ sp2) = compare (staticKey sp1) (staticKey sp2)

instance Show ActionSpec where
    showsPrec p (ActionSpec _ _ sp) = showParen (p > appPrec) $
        showString "ActionSpec " . showsPrec appPrec1 (staticPtrInfo sp)

data ActionType
    = Default
    | Task
    | Interrupt
    | Function
    | OpenCL
  deriving ( Bounded
           , Data
           , Enum
           , Eq
           , Generic
           , Ix
           , Ord
           , Read
           , Show
           )

instance C C'hpx_action_type_t ActionType where
    fromC v
        | v == C'HPX_DEFAULT   = Default
        | v == C'HPX_TASK      = Task
        | v == C'HPX_INTERRUPT = Interrupt
        | v == C'HPX_FUNCTION  = Function
        | v == C'HPX_OPENCL    = OpenCL
        | otherwise = error $ "C C'hpx_action_type_t ActionType error fromC: " ++ show v
    toC Default   = C'HPX_DEFAULT
    toC Task      = C'HPX_TASK
    toC Interrupt = C'HPX_INTERRUPT
    toC Function  = C'HPX_FUNCTION
    toC OpenCL    = C'HPX_OPENCL

type ActionAttribute = Word32

pattern NoAttribute :: ActionAttribute
pattern NoAttribute = C'HPX_ATTR_NONE

pattern Marshalled :: ActionAttribute
pattern Marshalled = C'HPX_MARSHALLED

pattern Pinned :: ActionAttribute
pattern Pinned = C'HPX_PINNED

pattern Internal :: ActionAttribute
pattern Internal = C'HPX_INTERNAL

pattern Vectored :: ActionAttribute
pattern Vectored = C'HPX_VECTORED

pattern Coalesced :: ActionAttribute
pattern Coalesced = C'HPX_COALESCED

pattern Compressed :: ActionAttribute
pattern Compressed = C'HPX_COMPRESSED

pattern Null :: Action
pattern Null <- ((== Action c'HPX_ACTION_NULL) -> True) where
    Null = Action c'HPX_ACTION_NULL

pattern Invalid :: Action
pattern Invalid <- ((== Action c'HPX_ACTION_INVALID) -> True) where
    Invalid = Action c'HPX_ACTION_INVALID

newtype Address = Address C'hpx_addr_t
  deriving ( Bits
           , Bounded
           , Data
           , Enum
           , Eq
           , FiniteBits
           , Generic
           , Integral
           , Ix
           , Num
           , Ord
           , PrintfArg
           , Read
           , Real
           , Show
           , Storable
           )

newtype Status = Status C'hpx_status_t
  deriving ( Bits
           , Bounded
           , Data
           , Enum
           , Eq
           , FiniteBits
           , Generic
           , Integral
           , Ix
           , Num
           , Ord
           , PrintfArg
           , Read
           , Real
           , Show
           , Storable
           )

pattern Error :: Status
pattern Error <- ((== Status c'HPX_ERROR) -> True) where
    Error = Status c'HPX_ERROR

pattern Success :: Status
pattern Success <- ((== Status c'HPX_SUCCESS) -> True) where
    Success = Status c'HPX_SUCCESS

pattern Resend :: Status
pattern Resend <- ((== Status c'HPX_RESEND) -> True) where
    Resend = Status c'HPX_RESEND

pattern LCOError :: Status
pattern LCOError <- ((== Status c'HPX_LCO_ERROR) -> True) where
    LCOError = Status c'HPX_LCO_ERROR

pattern LCOChanEmpty :: Status
pattern LCOChanEmpty <- ((== Status c'HPX_LCO_CHAN_EMPTY) -> True) where
    LCOChanEmpty = Status c'HPX_LCO_CHAN_EMPTY

pattern LCOTimeout :: Status
pattern LCOTimeout <- ((== Status c'HPX_LCO_TIMEOUT) -> True) where
    LCOTimeout = Status c'HPX_LCO_TIMEOUT

pattern LCOReset :: Status
pattern LCOReset <- ((== Status c'HPX_LCO_RESET) -> True) where
    LCOReset = Status c'HPX_LCO_RESET

pattern ENoMem :: Status
pattern ENoMem <- ((== Status c'HPX_ENOMEM) -> True) where
    ENoMem = Status c'HPX_ENOMEM

pattern User :: Status
pattern User <- ((== Status c'HPX_USER) -> True) where
    User = Status c'HPX_USER
