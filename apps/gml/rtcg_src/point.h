
#ifndef __POINT_H
#define __POINT_H

#include "top.h"

#define color Point::point

prefix Point {
open Point;

extern struct point { FP x; FP y; FP z; }

extern void print(point p);

extern point add(point p1, point p2);
extern FP dot(point p1, point p2);
extern point neg(point p);
extern point sub(point p1, point p2);
extern point mul(point p1, point p2);
extern point inv(point p);
extern point scale(FP s, point p);
extern point shift(FP s, point p);
extern point normalize(point p);

extern FP length(point p);
}
#endif

