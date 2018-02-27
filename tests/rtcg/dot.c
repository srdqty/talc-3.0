#include <stdio.h>
#include <stdlib.h>
#include "ctiming.h"
#include "popc.h"

#ifdef POP
int dot (ARR_TYP(int,u), ARR_TYP(int,v)) {
#else
  int dot (int len, ARR_TYP(int,u), ARR_TYP(int,v)) {
#endif
  PONLY(int len = ARR_SIZE(u);)
  int result = 0;
  int i;

  for(i = 0; i < len; i++)
    result += ARR_GET(u,i) * ARR_GET(v,i);
  return result;
} 

#ifdef POP
#  define DOT(X,U,V) dot(U,V)
#else
#  define DOT(X,U,V) dot(X,U,V)
#endif

char * strconcat(char *s1, char *s2) {
  int s1_len = strlen(s1);
  int s2_len = strlen(s2);
  int i = 0;
  char * result = malloc(sizeof(char) * (s1_len + s2_len + 1));

  for(i=0; i<s1_len; i++)
    result[i] = s1[i];
  for(i=0;i<s2_len;i++)
    result[i+s1_len] = s2[i];
  result[s1_len+s2_len] = '\0';
  
  return result;
}

int main(int argc, char **argv) {

  int vec_len, number_zeroes,percent_zeroes;
  char *args;
  int result;

  if (argc!=3) 
    {
      printf("%s: usage: <vec_len> <percent zeroes>\n",argv[0]);
      return 0;
    } 

  vec_len = atoi(argv[1]);
  percent_zeroes = atoi(argv[2]);
  number_zeroes = (vec_len * percent_zeroes)/100;

  if(number_zeroes < 0 || number_zeroes > vec_len) {
    printf("Illegal percentage %d\n",percent_zeroes);
    return 0;
  }

  { 
    ARR_DECL(int,u,vec_len,0);
    int i;
    for(i = 0; i < vec_len; i++) {
      ARR_SET(u,i,(i+1));
    }
    for(i = 0; i < number_zeroes; i++) {
      ARR_SET(u,i,0);
    }
    
    args = strconcat(argv[1],strconcat(" ",argv[2]));

    MAIN_TEST("dot",C_COMPILER,PONLY(POPC_KIND) CONLY(PLAIN_KIND),
	      args,0,DOT(vec_len,u,u));
    
    result = DOT(vec_len,u,u);  
    printf("vec_len = %d, result = %d\n",vec_len, result);
  }
  
  return 0;
}
