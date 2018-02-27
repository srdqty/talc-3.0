// fft
// Copied from Lawall's paper.
// Faster Fourier Transforms via Automatic Program Specialization
//
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "ctiming.h"
#include "popc.h"

#define PI (3.141592654)

#define FP double

typedef ARR_TYP(FP,fp_arr);

POP_ARRAY_DEF(fp_arr);

void scramble(ARR_TYP(FP,fr), ARR_TYP(FP,fi), int n) {
  int m,k,j;

  for(m=1,j=0; m<n-1;m++) {
    for(k=n>>1; ((j^=k)&k) == 0; k>>=1) 
      /* Do nothing. */;

    if(j>m) {
      FP tmp;
      tmp = ARR_GET(fr,m);
      ARR_SET(fr,m, ARR_GET(fr,j));
      ARR_SET(fr,j,tmp);
      
      tmp = ARR_GET(fi,m);
      ARR_SET(fi,m, ARR_GET(fi,j));
      ARR_SET(fi,j,tmp);
    }
  }
}

// is = 1 == fft, is = -1 == inverse fft
// The inverse does not preserve amplitude -- divide by the length of the array!
// fr and fi are the real and imaginary parts of the input respectively.
// ldn = log_2 (size(fr))
void fft(ARR_TYP(FP,fr), ARR_TYP(FP,fi), int ldn, int is) {

  int n2 = 1 << ldn;
  FP is_pi = is * PI;
  int ldm,j,r;

  scramble(fr,fi,n2);

  for(ldm=1; ldm <= ldn; ldm++) {
    int m = 1 << ldm;
    int mh = m >> 1;
    FP phi = is_pi / mh;
    FP w=0.0;
    for(j=0; j < mh; j++) {
      FP c,s;
      w += phi;
      c = cos(w);
      s = sin(w);


      for(r=0; r<n2; r+=m) {
	int t1 = r+j;
	int t2 = t1+mh;

	FP fr2 = ARR_GET(fr,t2), fi2=ARR_GET(fi,t2);
	FP vr = fr2 * c - fi2 * s;
	FP vi = fr2 * s + fi2 * c;

	FP ur = ARR_GET(fr,t1);
	FP ui = ARR_GET(fi,t1);

	ARR_SET(fr,t1,ur+vr);
	ARR_SET(fr,t2,ur-vr);
	
	ARR_SET(fi,t1,ui+vi);
	ARR_SET(fi,t2,ui-vi);
      }
    }
  }
}

#ifdef POP
void prn_fp_arrays(ARR_TYP(FP,fr), ARR_TYP(FP,fi)) 
#else
void prn_fp_arrays(int n, ARR_TYP(FP,fr), ARR_TYP(FP,fi))
#endif
{
  PONLY(int n = ARR_SIZE(fr);)
  int i;
  for(i = 0; i < n; i++) {
    printf("%d \t (%g + %gi) \t |%g|\n",i,ARR_GET(fr,i),ARR_GET(fi,i),sqrt(ARR_GET(fr,i)*ARR_GET(fr,i) + ARR_GET(fi,i) * ARR_GET(fi,i)));
  }
  printf("\n");
}

#ifdef POP
#define PRN_FP_ARRAYS(N,FR,FI) prn_fp_array(FR,FI)
#else
#define PRN_FP_ARRAYS(N,FR,FI) prn_fp_arrays(N,FR,FI)
#endif

#ifdef POP
ARR_TYP(FP,fp_copy(ARR_TYP(FP,x)))
#else
ARR_TYP(FP, fp_copy(int len,ARR_TYP(FP,x)))
#endif
{
  PONLY(int len = ARR_SIZE(x);)
  ARR_DECL(FP,y,len,0.0);
  int i;

  for(i=0; i<len;i++) {
    ARR_SET(y,i, ARR_GET(x,i));
  }

  return y;
}

#ifdef POP
#define FP_COPY(N,X) fp_copy(X)
#else
#define FP_COPY(N,X) fp_copy(N,X)
#endif


#ifdef POP
void mean_error(ARR_TYP(FP,fr), ARR_TYP(FP,fi), 
		ARR_TYP(FP,gr), ARR_TYP(FP,gi)) 
#else
void mean_error(int len,ARR_TYP(FP,fr), ARR_TYP(FP,fi), 
		ARR_TYP(FP,gr), ARR_TYP(FP,gi)) 
#endif
{
  PONLY(int len = ARR_SIZE(fr);)
  ARR_DECL(FP,err,len,0.0);
  int i;

  for(i=0; i<len;i++) {
    FP r_err = ARR_GET(fr,i) - ARR_GET(gr,i);
    FP i_err = ARR_GET(fi,i) - ARR_GET(gi,i);
    
    ARR_SET(err,i,sqrt(r_err * r_err + i_err * i_err));
  }
 
  for(i=0; i<len; i++) {
    if(ARR_GET(err,i) < 1e-10) printf("0 ");
    else printf("%g ",ARR_GET(err,i));
  }

  printf("\n");
}

int main (int argc, char **argv) {

  int len_log,len;
  if(argc!=2) {
    printf("usage: %s <log of input length>\n",argv[0]);
    return -1;
  }

  len_log = atoi(argv[1]);
  len     = 1 << len_log;

  if(len_log <=0 || len_log >=32) {
    printf("Input length must be betwee 1 and 31.\n");
    return -1;
  }

  {
    ARR_DECL(FP,fi,len,0.0);
    ARR_DECL(FP,fr,len,0.0);
    ARR_DECL(fp_arr,fr_data,100,0.0);
    ARR_DECL(fp_arr,fi_data,100,0.0);
    ARR_TYP(FP,fr_orig);
    ARR_TYP(FP,fi_orig);
    int i;

    for(i=0; i<len; i++) {
      double phi = (i/(double)(len-1)) * 2.0 * PI;
      
      ARR_SET(fr,i, sin(phi) + sin(4.0 * phi));
      ARR_SET(fi,i,0);
    }


    fr_orig = FP_COPY(len,fr);
    fi_orig = FP_COPY(len,fi);

    for(i=0; i<100; i++) {
      ARR_SET(fr_data,i,FP_COPY(len,fr));
      ARR_SET(fi_data,i,FP_COPY(len,fi));
    }
    
    MAIN_TEST("fft",C_COMPILER,PONLY(POPC_KIND) CONLY(PLAIN_KIND),
	      argv[1],0,fft(fr,fi,len_log,1));
  }

  return 0;
}

/* Use pop_main for correctness testing!
int pop_main () {

  int len = 32, len_log = 5;
  FP  fi[] = new_array_double(len);
  FP  fr[] = new_array_double(len);

  for(int i=0; i<len; i++) {
    double phi = (i/(:double)(len-1)) * 2.0 * pi;

    fr[i] = sin(phi) + sin(4.0 * phi);
    fi[i] = 0;
  }

  FP fr_orig[] = fp_copy(fr);
  FP fi_orig[] = fp_copy(fi);

  fft(fr,fi,len_log,1);

  prn_fp_arrays(fr,fi);

  fft(fr,fi,len_log,-1);

  for(int i=0;i<len;i++) {
    fr[i] /= len;
    fi[i] /= len;
  }

  mean_error(fr_orig,fi_orig,fr,fi);

  return 0;
}
*/
