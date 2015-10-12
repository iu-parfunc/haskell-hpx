#ifndef WR_HPX_H
#define WR_HPX_H

#include <hpx/hpx.h>

hpx_type_t wr_hpx_char();
hpx_type_t wr_hpx_uchar();
hpx_type_t wr_hpx_schar();
hpx_type_t wr_hpx_short();
hpx_type_t wr_hpx_ushort();
hpx_type_t wr_hpx_sshort();
hpx_type_t wr_hpx_int();
hpx_type_t wr_hpx_uint();
hpx_type_t wr_hpx_sint();
hpx_type_t wr_hpx_long();
hpx_type_t wr_hpx_ulong();
hpx_type_t wr_hpx_slong();
hpx_type_t wr_hpx_void();
hpx_type_t wr_hpx_uint8();
hpx_type_t wr_hpx_sint8();
hpx_type_t wr_hpx_uint16();
hpx_type_t wr_hpx_sint16();
hpx_type_t wr_hpx_uint32();
hpx_type_t wr_hpx_sint32();
hpx_type_t wr_hpx_uint64();
hpx_type_t wr_hpx_sint64();
hpx_type_t wr_hpx_float();
hpx_type_t wr_hpx_double();
hpx_type_t wr_hpx_pointer();
hpx_type_t wr_hpx_longdouble();
// hpx_type_t wr_hpx_complex_float();
// hpx_type_t wr_hpx_complex_double();
// hpx_type_t wr_hpx_complex_longdouble();
hpx_type_t wr_hpx_size();
hpx_type_t wr_hpx_addr();
hpx_type_t wr_hpx_action();

hpx_addr_t wr_hpx_null();
hpx_addr_t wr_hpx_here();

void       wr_hpx_time_now(hpx_time_t *dest);
uint64_t   wr_hpx_time_to_ns(const hpx_time_t *t);
double     wr_hpx_time_us(const hpx_time_t *from);
double     wr_hpx_time_ms(const hpx_time_t *from);
double     wr_hpx_time_diff_us(const hpx_time_t *from, const hpx_time_t *to);
double     wr_hpx_time_diff_ms(const hpx_time_t *from, const hpx_time_t *to);
int64_t    wr_hpx_time_diff_ns(const hpx_time_t *from, const hpx_time_t *to);
void       wr_hpx_time_diff(const hpx_time_t *start, const hpx_time_t *end, hpx_time_t *diff);
double     wr_hpx_time_elapsed_us(const hpx_time_t *from);
double     wr_hpx_time_elapsed_ms(const hpx_time_t *from);
uint64_t   wr_hpx_time_elapsed_ns(const hpx_time_t *from);
void       wr_hpx_time_elapsed(const hpx_time_t *start, hpx_time_t *diff);
void       wr_hpx_time_construct(hpx_time_t *dest, const unsigned long s, const unsigned long ns);
void       wr_hpx_time_point(hpx_time_t *dest, const hpx_time_t *time, const hpx_time_t *duration);

#endif
