#ifndef __MATRIX_H
#define __MATRIX_H

#include "top.h"
#include "core.h"
prefix Matrix {
open Matrix;

extern struct matrix {
  FP x0,x1,x2,x3;
  FP y0,y1,y2,y3;
  FP z0,z1,z2,z3;
  FP a0,a1,a2,a3;
}

extern struct vector { FP x0,x1,x2,x3; }

extern matrix identity;

extern matrix translate(FP x, FP y, FP z);

extern matrix unscale(FP x, FP y, FP z);
extern matrix scale(FP x, FP y, FP z);
extern matrix unuscale(FP s);
extern matrix uscale(FP s);
extern matrix rotatex(FP t);
extern matrix rotatey(FP t);
extern matrix rotatez(FP t);

extern matrix mul(matrix,matrix);
extern matrix transpose(matrix);
extern void vmul(vector,matrix,vector);
extern vector add_scaled(vector u,FP t,vector v); // u + t*v
extern void imp_add_scaled(vector u, FP t, vector v); // u += t*v

extern vector add(vector u, vector v);
 extern void sub(vector r, vector u, vector v); // r = u - v
extern FP dot(vector u, vector v); // u . v   (dot is prod in their code.)
extern FP square(vector y); // u . u

 extern vector normalize(vector r,vector u); /* Returns r! */
extern vector neg(vector x);

extern void print(FILE f,matrix m);
extern void vprint(FILE f,vector v);

}

#endif 
