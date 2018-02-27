#include "ctiming.h"
#include "popc.h"

double pow2(int n, double x) {
  double t = x;
  double result = 1.0;
  while(n!=0) {
    if((n & 1)==1) result *= t;
    t = t * t;
    n = n >> 1;
  }
  return result;
}

int main(int argc, char *argv[]) {
  int exp;
  double result = 0;
  double x = 0.8;
  int i;

  if(argc!=2) {
    printf("%s: usage <exponent>\n",argv[0]);
    return 1;
  }

  exp = atoi(argv[1]);

  MAIN_TEST("powf2",C_COMPILER,PONLY(POPC_KIND) CONLY(PLAIN_KIND),
	    argv[1],0,pow2(exp,x));

  result = pow2(exp,x);
  printf("exponent = %d, result = %g\n",exp,result);
  
  return 0;
}
