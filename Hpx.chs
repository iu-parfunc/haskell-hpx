


module Hpx where

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

hpxInit :: [String] -> IO Int
hpxInit s = fmap fromIntegral $
            {# call hpx_init #} nullPtr nullPtr

