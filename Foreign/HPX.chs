


module Foreign.HPX
       
       where

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C2HS

import System.Environment

#include "hpx/hpx.h"

-- include "hpx/rpc.h"

-- int hpx_init(int *argc, char ***argv);

-- foreign import hpx_init :: Ptr Int -> Ptr String -> IO Int

-- {# fun unsafe hpx_init
--   { `Ptr CInt'
-- --  , `Ptr (Ptr String)' 
--   , `Ptr (Ptr (Ptr CChar))' }
--     -> `CInt'   #}


type Action     = {#type hpx_action_t#}
type Handler    = {#type hpx_action_handler_t#}

-- hpxRegisterAction :: String -> Handler -> IO Action
-- hpxRegisterAction s h = {# call hpx_register_action #} s h

{# fun unsafe hpx_register_action as ^
 { withCString* `String'
 , id           `Handler'
 } -> `CULong' id #}


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

