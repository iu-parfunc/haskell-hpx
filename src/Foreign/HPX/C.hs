{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Foreign.HPX.C where

import Bindings.HPX
import Foreign.HPX.Types

class C c h where
    fromC :: c -> h
    toC   :: h -> c

instance C C'hpx_action_type_t ActionType where
    fromC v
        | v == c'HPX_DEFAULT   = Default
        | v == c'HPX_TASK      = Task
        | v == c'HPX_INTERRUPT = Interrupt
        | v == c'HPX_FUNCTION  = Function
        | v == c'HPX_OPENCL    = OpenCL
        | otherwise = error $ "C C'hpx_action_type_t ActionType error fromC: " ++ show v
    toC Default   = c'HPX_DEFAULT
    toC Task      = c'HPX_TASK
    toC Interrupt = c'HPX_INTERRUPT
    toC Function  = c'HPX_FUNCTION
    toC OpenCL    = c'HPX_OPENCL
