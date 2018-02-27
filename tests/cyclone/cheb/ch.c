#include "cheb.h"
#include <math.h>

float c[MAX];

void cheb(float c[MAX], int, float, float);
extern float func(float);
extern double cos(double x);

/*
float func_c(float x)
{
  return 2*x*x + x + 3;
}
*/

void cheb_c(float func_c(float), float c[MAX], int n, float xa, float xb)
{
  int k, j;
  float xm, xp, sm;
  float f[MAX];

  xp = (xb + xa) / 2;
  xm = (xb - xa) / 2;

  for(k = 1; k <= n; k++) {
    f[k] = func_c(xp + xm * (float)cos(PI * (k - 0.5) / n));
  }

  for(j = 0; j <= n-1; j++) {
    sm = 0.0;
    for(k = 1; k <= n; k++) {
      sm = sm + f[k] * (float)cos(PI * (float)j * ((float)k - 0.5) / (float)n);
    }
    c[j] = (float)(2.0 / (float)n) * sm;
  } 
  return;
}

void print_vector_c(float c[MAX], int start, int stop)
{
  int i;

  for(i = start; i < stop; i++)
    printf("%4.4f ", c[i]);
  printf("\n");
}

test_c(float func_c(float))
{
  cheb_c(func_c, c, 5, 0.0, 2.0);
  print_vector_c(c, 0, 5);
  cheb_c(func_c, c, 5, 0.0, 3.0);
  print_vector_c(c, 0, 5);
  cheb_c(func_c, c, 5, 0.0, 4.0);
  print_vector_c(c, 0, 5);

  cheb_c(func_c, c, 5, 1.0, 2.0);
  print_vector_c(c, 0, 5);
  cheb_c(func_c, c, 5, 1.0, 3.0);
  print_vector_c(c, 0, 5);
  cheb_c(func_c, c, 5, 1.0, 4.0);
  print_vector_c(c, 0, 5);
}
