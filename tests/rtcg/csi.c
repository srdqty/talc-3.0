// Cubic spline interpolation
// copied and modified from Tempo 
// based on code by Glueck et. al.
// I don't understand what this does exactly.
#include <stdio.h>
#include <stdlib.h>
#include "ctiming.h"
#include "popc.h"

/* csi(S, D, D); */
/* All the arrays are the same size. */
/* I couldn't tell you why we want to specialize on the x-components? 
   But its damn effective. */
#ifdef POP
void csi(ARR_TYP(float,x), ARR_TYP(float,y), ARR_TYP(float,z))
#else
void csi(int n, ARR_TYP(float,x), ARR_TYP(float,y), ARR_TYP(float,z))
#endif
{
  PONLY(int n = ARR_SIZE(x) - 1;)
  /* For performance, should probably make these arrays static. */
  ARR_DECL(float,h,(n+1),0.0);
  ARR_DECL(float,b,(n+1),0.0);
  ARR_DECL(float,u,(n+1),0.0);
  ARR_DECL(float,v,(n+1),0.0);

  int i = 0;
  for(; i<= n-1; i=i+1){
    ARR_SET(h,i, ARR_GET(x,(i+1)) - ARR_GET(x,i));
    ARR_SET(b,i, (6/ARR_GET(h,i))*(ARR_GET(y,(i+1))-ARR_GET(y,i)));
  }

  ARR_SET(u,1,2*(ARR_GET(h,0)+ARR_GET(h,1)));
  ARR_SET(v,1, ARR_GET(b,1)-ARR_GET(b,0));
  for (i = 2; i <= n-1; i=i+1){
    ARR_SET(u,i, 2*(ARR_GET(h,i)+ARR_GET(h,(i-1)))-ARR_GET(h,(i-1))*ARR_GET(h,(i-1))/ARR_GET(u,(i-1)));
    ARR_SET(v,i, ARR_GET(b,i)-ARR_GET(b,(i-1))-ARR_GET(h,(i-1))*ARR_GET(v,(i-1))/ARR_GET(u,(i-1)));
  }

  ARR_SET(z,n,0); // Why n? not n-1?

  i=n-1;
  if ( i>=1 )
    do {
      ARR_SET(z,i,(ARR_GET(v,i)-ARR_GET(h,i)*ARR_GET(z,(i+1)))/ARR_GET(u,i));
      i=i-1;
    } while ( i>=1 );
  ARR_SET(z,0,0);
}

#ifdef POP
#define CSI(N,X,Y,Z) csi(X,Y,Z)
#else
#define CSI(N,X,Y,Z) csi(N,X,Y,Z)
#endif

int main(int argc, char **argv) {
  
  int len;

  if(argc!=2) {
    printf("usage: %s <num points>\n",argv[0]);
    return 1;
  }

  len = atoi(argv[1]);
  if(len<=0) {
    printf("number of points %d <= 0\n",len);
    return 1;
  }

  {
    int i;
    ARR_DECL(float,x,len,0.0);
    ARR_DECL(float,y,len,0.0);
    ARR_DECL(float,z,len,0.0);

    for(i=0;i < len; i++) {
      ARR_SET(x,i,i);
      ARR_SET(y,i,i*i);
    }

    MAIN_TEST("csi",C_COMPILER,PONLY(POPC_KIND) CONLY(PLAIN_KIND),
	      argv[1],0,CSI(len-1,x,y,z));
    
#ifdef DEBUG
  printf("Result:");
  for(i=0; i<len; i++) {
    printf("%g ",ARR_GET(z,i));
  }
  printf("\n");
#endif
  }
  return 0;
}
