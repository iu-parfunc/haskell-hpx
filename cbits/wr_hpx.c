#include <hpx/hpx.h>
#include "wr_hpx.h"

hpx_type_t wr_hpx_char() {
  return HPX_CHAR;
}

hpx_type_t wr_hpx_uchar() {
  return HPX_UCHAR;
}

hpx_type_t wr_hpx_schar() {
  return HPX_SCHAR;
}

hpx_type_t wr_hpx_short() {
  return HPX_SHORT;
}

hpx_type_t wr_hpx_ushort() {
  return HPX_USHORT;
}

hpx_type_t wr_hpx_sshort() {
  return HPX_SSHORT;
}

hpx_type_t wr_hpx_int() {
  return HPX_INT;
}

hpx_type_t wr_hpx_uint() {
  return HPX_UINT;
}

hpx_type_t wr_hpx_sint() {
  return HPX_SINT;
}

hpx_type_t wr_hpx_long() {
  return HPX_LONG;
}

hpx_type_t wr_hpx_ulong() {
  return HPX_ULONG;
}

hpx_type_t wr_hpx_slong() {
  return HPX_SLONG;
}

hpx_type_t wr_hpx_void() {
  return HPX_VOID;
}

hpx_type_t wr_hpx_uint8() {
  return HPX_UINT8;
}

hpx_type_t wr_hpx_sint8() {
  return HPX_SINT8;
}

hpx_type_t wr_hpx_uint16() {
  return HPX_UINT16;
}

hpx_type_t wr_hpx_sint16() {
  return HPX_SINT16;
}

hpx_type_t wr_hpx_uint32() {
  return HPX_UINT32;
}

hpx_type_t wr_hpx_sint32() {
  return HPX_SINT32;
}

hpx_type_t wr_hpx_uint64() {
  return HPX_UINT64;
}

hpx_type_t wr_hpx_sint64() {
  return HPX_SINT64;
}

hpx_type_t wr_hpx_float() {
  return HPX_FLOAT;
}

hpx_type_t wr_hpx_double() {
  return HPX_DOUBLE;
}

hpx_type_t wr_hpx_pointer() {
  return HPX_POINTER;
}

hpx_type_t wr_hpx_longdouble() {
  return HPX_LONGDOUBLE;
}

// hpx_type_t wr_hpx_complex_float() {
//   return HPX_COMPLEX_FLOAT;
// }
//
// hpx_type_t wr_hpx_complex_double() {
//   return HPX_COMPLEX_DOUBLE;
// }
//
// hpx_type_t wr_hpx_complex_longdouble() {
//   return HPX_COMPLEX_LONGDOUBLE;
// }

hpx_type_t wr_hpx_size() {
  return HPX_SIZE_T;
}

hpx_type_t wr_hpx_addr() {
  return HPX_ADDR;
}

hpx_type_t wr_hpx_action() {
  return HPX_ACTION_T;
}

hpx_addr_t wr_hpx_null() {
  return HPX_NULL;
}

hpx_addr_t wr_hpx_here() {
  return HPX_HERE;
}

void wr_hpx_time_now(hpx_time_t *dest) {
  *dest = hpx_time_now();
}

uint64_t wr_hpx_time_to_ns(const hpx_time_t *t) {
  return hpx_time_to_ns(*t);
}

double wr_hpx_time_us(const hpx_time_t *from) {
  return hpx_time_us(*from);
}

double wr_hpx_time_ms(const hpx_time_t *from) {
  return hpx_time_ms(*from);
}

double wr_hpx_time_diff_us(const hpx_time_t *from, const hpx_time_t *to) {
  return hpx_time_diff_us(*from, *to);
}

double wr_hpx_time_diff_ms(const hpx_time_t *from, const hpx_time_t *to) {
  return hpx_time_diff_ms(*from, *to);
}

int64_t wr_hpx_time_diff_ns(const hpx_time_t *from, const hpx_time_t *to) {
  return hpx_time_diff_ns(*from, *to);
}

void wr_hpx_time_diff(const hpx_time_t *start, const hpx_time_t *end, hpx_time_t *diff) {
  hpx_time_diff(*start, *end, diff);
}

double wr_hpx_time_elapsed_us(const hpx_time_t *from) {
  return hpx_time_elapsed_us(*from);
}

double wr_hpx_time_elapsed_ms(const hpx_time_t *from) {
  return hpx_time_elapsed_ms(*from);
}

uint64_t wr_hpx_time_elapsed_ns(const hpx_time_t *from) {
  return hpx_time_elapsed_ns(*from);
}

void wr_hpx_time_elapsed(const hpx_time_t *start, hpx_time_t *diff) {
  hpx_time_elapsed(*start, diff);
}

void wr_hpx_time_construct(hpx_time_t *dest, const unsigned long s, const unsigned long ns) {
  *dest = hpx_time_construct(s, ns);
}

void wr_hpx_time_point(hpx_time_t *dest, const hpx_time_t *time, const hpx_time_t *duration) {
  *dest = hpx_time_point(*time, *duration);
}

// Extra C utilities that we call from Haskell:
// --------------------------------------------------------------------------------

// A place to put extra initialization.  For now, register the
// singular Haskell upcall funptr, which we assume to have been
// written already.
void hs_hpx_extra_init() {
  //  hpx_register_action();
  // ((void*())_HS_UPCALL)()
}

typedef void* hs_upcall_t;

// Here we use a global variable as a place to stash a function
// pointer for the single Haskell upcall we currently need.
void* _HS_UPCALL = 0;

void set_hs_upcall(void* arg) {
  _HS_UPCALL = arg;
}
