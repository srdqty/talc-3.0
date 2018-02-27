// Stolen from Tempo
#include <stdio.h>
#include <stdlib.h>
#include "ctiming.h"
#include "popc.h"
#include <math.h>

#define PI 3.141592654

static float mycos(float x) {
  return cos(x);
}

// c is the result array. Specialize on size of c.
// Tempo set n < 50.  
// Tempo specializes on n only.
#ifdef POP
void cheb(float func(float), ARR_TYP(float,c), float xa, float xb)
#else
void cheb(int n, float func(float), ARR_TYP(float,c), float xa, float xb)
#endif
{
  PONLY(int n = ARR_SIZE(c);)

  int k, j;
  float xm, xp, sm;
  ARR_DECL(float,f,(n+1),0.0);

  xp = (xb + xa) / 2;
  xm = (xb - xa) / 2;

  for(k = 1; k <= n; k++) {
    ARR_SET(f,k,func(xp + xm * (float)cos(PI * (k - 0.5) / n)));
  }

  for(j = 0; j <= n-1; j++) {
    sm = 0.0;
    for(k = 1; k <= n; k++) {
      sm = sm + ARR_GET(f,k) * (float)cos(PI * (float)j * ((float)k - 0.5) / (float)n);
    }
    ARR_SET(c,j, (float)(2.0 / (float)n) * sm);
  } 
  return;
}

#ifdef POP
#define CHEB(N,F,C,XA,XB) cheb(F,C,XA,XB)
#else
#define CHEB(N,F,C,XA,XB) cheb(N,F,C,XA,XB)

#endif
int main(int argc, char **argv) {
  int len;
  if(argc!=2) {
    printf("usage: %s <length>\n",argv[0]);
    return 1;
  }

  len = atoi(argv[1]);

  if(len<=0 || len >= 100) {
    printf("Invalid length %d\n",len);
    return 1;
  }

  {
    ARR_DECL(float,c,(len+1),0.0);
  
    MAIN_TEST("cheb",C_COMPILER,PONLY(POPC_KIND) CONLY(PLAIN_KIND),argv[1],0,CHEB(len+1,mycos,c,1.0,3.0));

#ifdef DEBUG
  for(int i=0;i<len;i++) {
    printf("%g ",ARR_GET(c,i));
  }
  printf("\n");
#endif
  }
  return 0;
}

