#include "unixsupport.h"
#include <stdio.h>
#include <errno.h>
#include <string.h>

/***************************/
/* Converting to C strings */
/***************************/

/* If the string is null-terminated (a C-style string), then
   just return the buffer of the string itself.  Otherwise, 
   make a copy with the null terminator on the end. */

char *convert_pop_string(string s) {
  int i;
  for (i=0; i<s->size; i++) {
    if (s->chars[i] == '\0')
      return s->chars;
  }
  return string_to_Cstring(s);
}

/**************/
/* Allocation */
/**************/

extern void *GC_malloc(int);
extern void *GC_malloc_atomic(int);
extern void out_of_memory(void) Noreturn;

INLINE void *xalloc(int sizeb) {
  void *ret = GC_malloc(sizeb);
  if (ret == NULL)
    out_of_memory();
  else return ret;
}

INLINE void *xalloc_atomic(int sizeb) {
  void *ret = GC_malloc(sizeb);
  if (ret == NULL)
    out_of_memory();
  else return ret;  
}

array copy_poparray(void *(*f)(char *), char ** arr)
{
  int nbr, n;
  array result;

  nbr = 0;
  while (arr[nbr] != 0) nbr++;
  if (nbr == 0) {
    result = xalloc(sizeof(array));
    result->size = 0;
    return result;
  } else {
    result = xalloc (sizeof(array));
    result->size = nbr;
    result->elts = xalloc (sizeof(void *) * nbr);
    for (n = 0; n < nbr; n++) {
      ((void **)(result->elts))[n] = f(arr[n]);
    }
    return result;
  }
}

array copy_popstring_array(char **arr) {
  return copy_poparray(Cstring_to_string,arr);
}

/* Converts a Popcorn array of flags (a union type) to a single
   integer using bit tricks */
int convert_flags(array flags, int flag_table[]) {
  int i;
  int result = 0;
  for (i=0; i<flags->size; i++) {
    result |= flag_table[((int *)flags->elts)[i]-1];
  }
  return result;
}

/*******************/
/* Error reporting */
/*******************/

struct unix_exn_arg {
  int no;
  string syscall;
};

#define MAX_LOC_STR_LEN 100

extern struct pop_exn *Unix_error_pkt;
extern struct pop_exn *NullPointer_pkt;

void format_exn(struct pop_exn *exn, char *file, int line)
{
  char *loc_buf;

  loc_buf = xalloc_atomic(MAX_LOC_STR_LEN);
  sprintf(loc_buf,"%s:%d",file,line);

  exn->loc_str->size = strlen(loc_buf);
  exn->loc_str->chars = loc_buf;
}

struct pop_exn *make_unix_error(char *file, int line, int code, char *msg)
{
  format_exn(Unix_error_pkt,file,line);
  ((struct unix_exn_arg *)Unix_error_pkt->arg)->no = code;
  ((struct unix_exn_arg *)Unix_error_pkt->arg)->syscall->size = strlen(msg);
  ((struct unix_exn_arg *)Unix_error_pkt->arg)->syscall->chars = msg;
  return Unix_error_pkt;
}

struct pop_exn *get_unix_error(char *file, int line, char *msg)
{
  return make_unix_error(file,line,errno,msg);
}

string unix_error_string(int errcode) {
  return (Cstring_to_string(strerror(errcode)));
}

struct pop_exn *get_nullpointer_exn(char *file, int line)
{
  format_exn(NullPointer_pkt,file,line);
  NullPointer_pkt->arg = NULL;
  return NullPointer_pkt;
}
