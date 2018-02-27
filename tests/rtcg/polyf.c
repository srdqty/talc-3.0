#include "ctiming.h"
#include "popc.h"

/* We represent a polynomial as an array where the element at position i
   is the coefficient for exponent i. */

#ifdef POP
double poly(ARR_TYP(float,p), float x) {
#else
double poly(unsigned int len, ARR_TYP(float,p), float x) {
#endif
  PONLY(unsigned int len = ARR_SIZE(p);)
  unsigned int  i;
  float x_i = 1;
  double result = 0;

  for(i=0; i<len; ++i) {
    float ai = ARR_GET(p,i);
    if(ai != 0.0) result += ai * x_i;
    x_i *= x;
  }

  return result;
}

#ifdef POP
#   define POLY(X,U,V) poly(U,V)
#else
#   define POLY(X,U,V) poly(X,U,V)
#endif

int main(int argc, char **argv) {
  int exp;
  double result;
  float x = 0.8;
  int len;

  if(argc!=2) {
    printf("%s: usage <max exponent>\n",argv[0]);
    return 0;
  }

  exp = atoi(argv[1]);
  len = exp + 1;

  { 
    int i;
    ARR_DECL(float,p,len,0);

    for(i = 0; i < len; i++) {
      ARR_SET(p,i,i+1);
    }

    MAIN_TEST("polyf",C_COMPILER,PONLY(POPC_KIND) CONLY(PLAIN_KIND),
	      argv[1],0,POLY(len,p,x));

    result = POLY(len,p,x);
    printf("exponent = %d, result = %g\n",exp,result);
  }

  return 0;
}
