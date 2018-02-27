#ifndef __TOP_H
#define __TOP_H

/* definitions and types that are central to the whole enterprise. */

#define DOUBLES

// Keep in sync with top.pop!
#ifdef DOUBLES
#  define FP_CONST(X) X
#  define FP double
#  define print_fp print_double
#  define fprint_fp fprint_double
#  define new_array_fp new_array_double
#  define abs_fp fabs
#else
#  define FP_CONST(X) X##f
#  define FP float
#  define print_fp print_float
#  define fprint_fp fprint_float
#  define new_array_fp new_array_float
#  define abs_fp fabs
#endif

#define degree FP

extern void print_double(double);
extern void print_float(float);

extern FP deg_to_rad(FP deg);
extern FP rad_to_deg(FP rad);

extern exception GMLFailure(string);

#endif
