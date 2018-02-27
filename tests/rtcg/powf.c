#include "ctiming.h"
#include "popc.h"

double simple_pow(int n, double x) {
  double result = 1.0;
  while(n!=0) {
    result *=x;
    --n;
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

  MAIN_TEST("powf",C_COMPILER,PONLY(POPC_KIND) CONLY(PLAIN_KIND),
	    argv[1],0,simple_pow(exp,x));

  result = simple_pow(exp,x);
  printf("exponent = %d, result = %g\n",exp,result);
  
  return 0;
}
