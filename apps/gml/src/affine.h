
#ifndef __AFFINE_H
#define __AFFINE_H

#include "top.h"
#include "point.h"
#include "matrix.h"

prefix Affine {
open Affine;
 
 extern struct t {
   Matrix::matrix mat;
   Matrix::matrix imat; 
   bool isometric;        // Is this transformation isometric.
   FP factor;             // Scaling factor
 }

 extern t rotate_x(FP f);
 extern t rotate_y(FP f);
 extern t rotate_z(FP f);
 extern t uscale(FP f);
 
 extern t translate(FP x, FP y, FP z);
 extern t scale(FP x, FP y, FP z);
 extern t rotate(FP x, FP y, FP z);

 extern t compose(t t1,t t2);

 extern t invert(t);

}

#endif
