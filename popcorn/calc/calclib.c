#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

void *GC_malloc(int size);

typedef struct {int size; char chars[1];} *string;
typedef struct {int size; int elts[1];} *intarray;

string char_array_to_string(intarray ca,int len) 
{
  int i;

  string str=(string)GC_malloc(len+4);
  str->size = len;
  for (i=0; i < len; i++) {
    str->chars[i] = (char)(ca->elts[i]);
  }
  return(str);
}

void print_char(char c)
{
  printf("%c",c);
  return;
}

int tal_getchar() {
  return(getchar());
}

int tal_ungetchar(int i) {
  return(ungetc((char)i, stdin));
}

int eqstring(string x, string y) {
  int i;
  int len = x->size;

  if (len != y->size)
    return(0);

  for (i=0; i < len; i++) {
    if (x->chars[i] != y->chars[i])
      return(0);
  }
  return(1);
}
