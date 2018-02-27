#include "ctiming.h"
#include "popc.h"

int pow2(int n, int x) {
  int t = x;
  int result = 1;
  while(n!=0) {
    if((n & 1)==1) result *= t;
    t = t * t;
    n = n >> 1;
  }
  return result;
}

int main(int argc, char *argv[]) {
  int exp;
  int result = 0;
  int x = 15;
  int i;

  if(argc!=2) {
    printf("%s: usage <exponent>\n",argv[0]);
    return 1;
  }

  exp = atoi(argv[1]);

  MAIN_TEST("pow2",C_COMPILER,PONLY(POPC_KIND) CONLY(PLAIN_KIND),
	    argv[1],0,pow2(exp,x));

  result = pow2(exp,x);
  printf("exponent = %d, result = %d\n",exp,result);
  
  return 0;
}
