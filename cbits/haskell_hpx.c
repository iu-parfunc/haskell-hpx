#include <hpx/hpx.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "haskell_hpx.h"

int
haskell_hpx_trampoline(const hpx_action_t action)
{
    int res = 0;
    hpx_call_sync( HPX_HERE
                 , action
                 , &res
                 , sizeof(res)
                 , "" // The main action is of type IO (),
                 , 1  // so these "arguments" are ignored.
                 );

    // TODO: Figure out a good way to "return" something
    hpx_exit(EXIT_SUCCESS);
}
