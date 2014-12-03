


module Foreign.HPX where

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr

#include "hpx/hpx.h"

-- include "hpx/rpc.h"

-- int hpx_init(int *argc, char ***argv);

-- foreign import hpx_init :: Ptr Int -> Ptr String -> IO Int

-- {# fun unsafe hpx_init
--   { `Ptr CInt'
-- --  , `Ptr (Ptr String)' 
--   , `Ptr (Ptr (Ptr CChar))' }
--     -> `CInt'   #}

foreign import ccall "wrapper"
-- wrap :: (CDouble -> CDouble) -> IO (FunPtr (CDouble -> CDouble))
  wrap :: (Ptr () -> IO CInt) -> IO (FunPtr (Ptr () -> IO CInt))

foo :: Ptr () -> IO CInt
foo x = do
  print x
  return 0


type Action 	= {#type hpx_action_t#}
type Handler	= {#type hpx_action_handler_t#}

-- hpxRegisterAction :: String -> Handler -> IO Action
-- hpxRegisterAction s h = {# call hpx_register_action #} s h

{# fun unsafe hpx_register_action
 { withCString* `String'
 , id           `Handler'
 } -> `CULong' id #}

hpxInit :: [String] -> IO Int
hpxInit s = fmap fromIntegral $
            {# call hpx_init #} nullPtr nullPtr
            
