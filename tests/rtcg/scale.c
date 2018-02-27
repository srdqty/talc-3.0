#include <stdio.h>
#include <stdlib.h>
#include "ctiming.h"
#include "popc.h"

#ifdef POP
int scale(ARR_TYP(int,u),int v)
#else
int scale(int len,ARR_TYP(int,u),int v)
#endif
{
  PONLY(int len = ARR_SIZE(u);)
  int result = 0;
  int i;

  for(i = 0; i < len; ++i)
    result += ARR_GET(u,i) * v;
  return result;
} 

#ifdef POP
#define SCALE(N,U,V) scale(U,V)
#else 
#define SCALE(N,U,V) scale(N,U,V)
#endif

int main(int argc, char **argv)
{
  int vec_len;
  int x = 5;
  int result;

  if (argc!=2) 
    {
      printf("%s: usage: <vec_len>\n",argv[0]);
      return 1;
    } 

  vec_len = atoi(argv[1]);
  
  {
    ARR_DECL(int,u,vec_len,0);
    int i;

    for(i = 0; i < vec_len; i++) {
      ARR_SET(u,i,i);
    }

    MAIN_TEST("scale",C_COMPILER,PONLY(POPC_KIND) CONLY(PLAIN_KIND),
	      argv[1],0,SCALE(vec_len,u,x));
    
    result = SCALE(vec_len,u,x);
    printf("vec_len = %d, result = %d\n",vec_len, result);
  }
  return 0;
}

