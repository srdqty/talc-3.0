#include "ctiming.h"
#include "popc.h"

int simple_pow(int n, int x) {
  int result = 1;
  while(n!=0) {
    result *=x;
    --n;
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

  MAIN_TEST("pow",C_COMPILER,PONLY(POPC_KIND) CONLY(PLAIN_KIND),
	    argv[1],0,simple_pow(exp,x));

  result = simple_pow(exp,x);
  printf("exponent = %d, result = %d\n",exp,result);
  
  return 0;
}
