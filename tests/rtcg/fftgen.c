// This program generates a specialized version of fft.

#include <stdlib.h>
#include <stdio.h>

#define PI (3.141592654)
#define FP double

void fft_gen(FILE *f,int ldn, int is) {
  
  int n2 = 1 << ldn;
  FP is_pi = is * PI;
  int m,j,k,ldm;

  fprintf(f,"void fft(FP fr[], FP fi[]) {\n");

  // scramble inlined.
  // It is not at all clear that this pays off!
   for(m=1,j=0; m<n2-1;m++) {
     for(k=n2>>1; ((j^=k)&k) == 0; k>>=1) 
       ;
     
     if(j>m) {
       fprintf(f,
	       "{ FP tmp;\n"
	       "int m = %d;\n"
	       "int j = %d;\n"
	      " tmp = fr[m];\n"
	       "fr[m] = fr[j];\n"
	       "fr[j] = tmp;\n"
	       "\n"
	       "tmp = fi[m];\n"
	       "fi[m] = fi[j];\n"
	       "fi[j] = tmp;\n"
	       "}\n\n"
	       ,m,j);
     }
   }
      
   for(ldm=1; ldm <= ldn; ldm++) {
     int m = 1 << ldm;
     int mh = m >> 1;
     FP phi = is_pi / mh;
     FP w = 0.0;

     for(j=0; j < mh; j++, w+=phi) {
       FP c = cos(w);
       FP s = sin(w);
       
       fprintf(f,
	       "{ FP c = %g;\n"
	       "FP s = %g;\n"
	       "\n"
	       "for(int r=0; r<%d; r+=%d) { \n"
	       "int t1 = r+ %d;\n"
	       "int t2 = r+ %d;\n"
	       "FP fr2 = fr[t2], fi2 = fi[t2];\n"
	       "FP vr = fr2 * c - fi2 * s;\n"
	       "FP vi = fr2 * s + fi2 * c;\n"
	       "\n"
	       "FP ur = fr[t1];\n"
	       "fr[t1] = ur+vr;\n"
	       "fr[t2] = ur-vr;\n"
	       "\n"
	       "FP ui = fi[t1];\n"
	       "fi[t1]=ui+vi;\n"
	       "fi[t2]=ui-vi;"
	       "}\n"
	       "}\n",c,s,n2,m,j,j+mh);
     }
   }

   fprintf(f,"\n}\n");
}

int main(int argc, char **argv) {

  char *filename;
  int len;
  FILE *fptr;

  if(argc != 3) {
    printf("usage: %s <filename> <len>\n",argv[0]);
    return 1;
  }

  filename = argv[1];
  len = atoi(argv[2]);

  if(len <= 0 || len >= 31) {
    printf("Illegal length %d\n",len);
    return 1;
  }

  
  fptr = fopen(filename,"w");

  if(fptr == NULL) {
    printf("Failed to open file %s\n",filename);
    return 1;
  }

  fft_gen(fptr,len,1);

  fprintf(fptr,"\n\nint len_log = %d;\n",len);

  fclose(fptr);

  return 0;
}




