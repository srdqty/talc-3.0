/* Popcorn Language Stuff
 *
 * Copyright Greg Morrisett, Neal Glew, Dan Grossman
 *   January 1999, all rights reserved
 *
 */

/* Now hacked by Dan and Fred for more informative debugging */

#include <stdlib.h>
#include <stdio.h>

extern void **tal_args;

extern void *GC_malloc(int);
extern void GC_free(void *);

typedef char *Cstring;
typedef struct str_internal {int size; char *chars;} *string;
Cstring string_to_Cstring(string);
extern FILE* tal_stderr;

extern void fprint_string(FILE *,string);
extern void fprint_int(FILE *,int);

/* Call stack related externs */

typedef struct cstk_struct { string hd; struct cstk_struct *tl; } cstk;

/* Type cstk is defined in prelude.tal
   Exn variable __zzzz_active_exn is defined in prelude.tal */
cstk *__zzzz_global_cstk;
cstk *__zzzz_active_cstk;

static void cstk_dump(FILE *fd) {
  cstk *x = __zzzz_active_cstk;
  string current;
  int counter;
  fprintf(fd, "Call stack (only instrumented functions).\n");
  while(x!=NULL) {
    current = x->hd;
    counter=0;
    x=x->tl;
    while(x!=NULL) {
      if(current == x->hd) counter++;
      else break;
      x=x->tl;
    }

    if(counter>0) fprintf(fd,"(%d)\t",counter+1);
    else fprintf(fd,"\t");
    fprint_string(fd,current);
    fprintf(fd,"\n");
  }

  fprintf(fd,".\n");
}

void pop_never_null()
{
  fprintf(stderr, "Null pointer exception. \n\n");
  exit(255);
}

void pop_debug_never_null(int i,string s) {
  fprintf(stderr,"Null pointer exception: ");
  fprint_string(tal_stderr,s);
  fprintf(stderr,", char ");
  fprint_int(tal_stderr,i);
  exit(255);
}

typedef struct {
  void *id;
  string info;
  void *value;
} * exn_ptr;

void pop_exn_info(FILE *fd, exn_ptr exn) {
  fprintf(fd, "exception: %p\n",exn);
  fprintf(fd, "Originating at: ");
  fprint_string(fd, exn->info);
  fprintf(fd, "\n");

  if(__zzzz_active_cstk != NULL) cstk_dump(fd);
}

void pop_exn_handler(exn_ptr exn) {
  fprintf(stderr,"Uncaught ");
  pop_exn_info(stderr,exn);
  exit(255);
}

void pop_exn_handler_verbose(void* exn)
{
  /*  string msg; */

 fprintf(stderr, "Uncaught exception: %p\n",exn);
 // callStackDump();

 fprintf(stderr, "Recovering exception name and location.\n");
 fprintf(stderr, "May cause a crash if code not compiled with verbose exceptions.\n\n");

 fflush(stderr);

 /* Temporarily disabled. -- changing memory format of exceptions.
 msg = (((exn_ptr) exn)->packet->s);

 fprint_string(tal_stderr,msg);
 fprintf(stderr, "\n");
 */

 exit(255);
}

void array_bounds_error(void)
{
  fprintf(stderr, "\narray bounds error!\n");
  exit(255);
}

void division_by_zero_error(void)
{
  fprintf(stderr, "\ndivision by zero error!\n");
  exit(255);
}
