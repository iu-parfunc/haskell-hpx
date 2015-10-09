#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "Safe_stub.h"
extern void __stginit_Safe(void);
#endif
#include <stdio.h>
#include "cycle.h"

void* last_stack = 0;
ticks last_time = 0;

void print_stack_pointer() {
  void* p = NULL;

  if (last_time > 0)
    printf("Approx cycles to get to Haskell and back to C: %llu\n", getticks() - last_time);

  printf("stack pointer: %p, diff from last %ld\n", (void*)&p, ((void*)&p) - last_stack);
  last_stack = (void*)&p;
  last_time = getticks();
}

int main(int argc, char *argv[])
{
    int i,j;
    hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
    hs_add_root(__stginit_Safe);
#endif

    for (j=0; j<10; j++) {
      last_time = 0;
      printf("Stack pointer before calling into HS:\n");
      print_stack_pointer();
      i = fibonacci_hs(42);
      printf("Result from Haskell: %d\n", i);
    }

    hs_exit();
    return 0;
}
