/* Copied from tempo */

#include <stdio.h>
#include <stdlib.h>
#include "ctiming.h"
#include "popc.h"
#include<math.h>

#define DEBUG

// This integrates the function f from a to b with 2^(2M) subdivisions
float romberg(float f(float), float a, float b, int M)
{
  int n, m, i;
  float h, s;
  ARR_DECL(float,r0,(M+1),0.0);
  ARR_DECL(float,r1,(M+1),0.0);
  ARR_TYP(float, tmp);

  h = b - a;
  ARR_SET(r0,0,(f(a) + f(b)) * h / 2.0);

  for (n = 1; n <= M; n++) {
    int lim;
    h = h / 2.0;
    s = 0.0;

    lim = 1 << n-1;
    for (i = 1; i <= lim; i++) { 
      s = s + f(a + (float)(2.0 * i - 1) * h);
    }
    
    ARR_SET(r1,0, ARR_GET(r0,0)/2.0 + h * s);
    
    for(m = 1; m <= n; m++) {
      float p = (1 << (2*m)) - 1;
      ARR_SET(r1,m, ARR_GET(r1,m-1) + 
	 (float)(1.0/p) * (ARR_GET(r1,m-1) - ARR_GET(r0,m-1)));
    }

    tmp = r0;
    r0 = r1;
    r1 = tmp;
  }

  return ARR_GET(r0,M);
}

float sqr(float x){
  return x*x;
}

float cube(float x){
  return x*x*x;
}

int main(int argc, char **argv) {

  int prec;
  float result;

  if(argc != 2) {
    printf("usage: %s <precision>\n",argv[0]);
    return 1;
  }

  prec = atoi(argv[1]);

  if(prec<=0 || prec >=16) {
    printf("Precision %d is not between 1 and 15\n",prec);
    return 1;
  }


  MAIN_TEST("romberg",C_COMPILER,PONLY(POPC_KIND) CONLY(PLAIN_KIND),argv[1],0,romberg(cube,0.0,1.0,prec));

#ifdef DEBUG
  result = romberg(cube,0.0,1.0,prec);
  printf("integral(x**3,0,1) = %g \n",result);
#endif
}


