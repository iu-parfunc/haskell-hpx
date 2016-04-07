{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foreign.HPX.Types where

import Bindings.HPX
import Data.Ix (Ix)
import Foreign

newtype Action = Action C'hpx_action_t
  deriving (Eq, Ord, Read, Show, Storable)

type ActionAttribute = Word

data ActionType
    = Default
    | Task
    | Interrupt
    | Function
    | OpenCL
  deriving (Bounded, Enum, Eq, Ix, Ord, Read, Show)
