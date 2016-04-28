#include <hpx/hpx.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

int haskell_hpx_main(const char *, const size_t);
int haskell_hpx_trampoline(const hpx_action_t);
int hello_world(const char *, const size_t);

hpx_action_t haskell_hpx_main_ptr       = HPX_ACTION_INVALID;
hpx_action_t haskell_hpx_trampoline_ptr = HPX_ACTION_INVALID;
hpx_action_t hello_world_ptr            = HPX_ACTION_INVALID;

int
haskell_hpx_main(const char *bytestring, const size_t bytestring_len)
{
    const char   message[11] = "HelloWorld";
    const size_t message_len = 11;
    _hpx_call_cc(HPX_HERE, hello_world_ptr, 2, message, message_len);
}

int
haskell_hpx_trampoline(const hpx_action_t action)
{
    int res = 0;
    _hpx_call_sync(HPX_HERE, action, &res, sizeof(res), 2, "", 1);

    // TODO: Figure out a good way to "return" something
    hpx_exit(HPX_SUCCESS);
}

int
hello_world(const char *bytestring, const size_t bytestring_len)
{
    printf("Bytestring: %s, bytestring length: %lu\n", bytestring, bytestring_len);
    _hpx_thread_continue(2, bytestring, bytestring_len);
}

int main(int argc, char **argv) {
    hpx_register_action( HPX_DEFAULT
                       , HPX_ATTR_NONE
                       , "haskell_hpx_trampoline:keyName"
                       , &haskell_hpx_trampoline_ptr
                       , 2
                       , haskell_hpx_trampoline
                       , HPX_ACTION_T
                       );
    hpx_register_action( HPX_DEFAULT
                       , HPX_MARSHALLED
                       , "haskell_hpx_main:keyName"
                       , &haskell_hpx_main_ptr
                       , 3
                       , haskell_hpx_main
                       , HPX_POINTER
                       , HPX_SIZE_T
                       );
    hpx_register_action( HPX_DEFAULT
                       , HPX_MARSHALLED
                       , "hello_world:keyName"
                       , &hello_world_ptr
                       , 3
                       , hello_world
                       , HPX_POINTER
                       , HPX_SIZE_T
                       );
    hpx_init(&argc, &argv);
    _hpx_run(&haskell_hpx_trampoline_ptr, 1, &haskell_hpx_main_ptr);
    hpx_finalize();
    return EXIT_SUCCESS;
}
