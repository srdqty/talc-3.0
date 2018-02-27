
#ifndef __MATH_H
#define __MATH_H

prefix Math {
open Math;

extern double pi;
extern double pos_inf;
extern double neg_inf;

extern double log2_e;
extern double log2_10;
extern double loge_2;
extern double log10_2;

/* Trignometric arguments in radians. */
extern double cos(double a); 
extern double sin(double a);
/* Returns the arc-tangent of x/y */
extern double atan(double x,double y);
extern double tan(double a);

extern double sqrt(double);

/* I don't know what these are for but the processor has them native. */
/* log ( y * log_2 x) */
extern double fyl2x(double y, double x);

/* log epsilon (y * log_2 (x + 1)) */
extern double fyl2xp1(double y, double x);

/* x should be in the range -1.0 to 1.0 */
/* Returns 2^x - 1 */
extern double f2xm1(double x);

extern double fabs(double x);

// Computes the IEEE remainder of x/y.  That is rounds x/y to the closest
// integer Q and returns x - Q*y.  This value is between -y/2 and y/2
extern double frem(double x, double y);

// Round x to the nearest double. 
// Should really set the rounding mode but assume it is the default.
extern double fround(double x);

//-----------------------------------------------------------------------------
// Below here the functions are not builtin

extern double floor(double x);

extern double frac(double x);
}
#endif
