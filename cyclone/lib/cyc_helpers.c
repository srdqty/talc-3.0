// This is the C "runtime library" to be used with the output of the
// Cyclone to C translator

// FIX: for now we use malloc instead of GC_malloc

#include <stdio.h>
#include <stdarg.h>
#include "cyc_include.h"

//////////////////////////////////////////////////////////
// First, definitions for things declared in cyc_include.h
//////////////////////////////////////////////////////////

char _Null_Exception_tag[15] = "Null_Exception";
struct _xenum_struct _Null_Exception_struct = { _Null_Exception_tag };
exn Null_Exception = &_Null_Exception_struct;
char _Match_Exception_tag[16] = "Match_Exception";
struct _xenum_struct _Match_Exception_struct = { _Match_Exception_tag };
exn Match_Exception = &_Match_Exception_struct;

struct _tagged_string *new_string(int sz) {
  struct _tagged_string *t;
  char *c;
  int i;

  if (sz < 0) {
    // FIX:  this should probably raise an exception
    fprintf(stderr,"new_string called with negative argument\n");
    exit(1);
  }

  t = (struct _tagged_string *)GC_malloc(sizeof(struct _tagged_string));
  if (t == NULL) {
    fprintf(stderr,"internal error: out of memory in new_string\n");
    exit(1);
  }
  // Include extra space for trailing 0, used in xprintf below
  c = (char *)GC_malloc(sz+1);
  if (c == NULL) {
    fprintf(stderr,"internal error: out of memory in new_string\n");
    exit(1);
  }
  i = 0;
  // Zero the array -- not needed if we use the proper malloc...
  for (; i <= sz; i++) c[i] = 0; // NB we clobber the extra space too
  t->sz = sz;
  t->contents = c;
  return t;
}

#define XPRINTF_BUFFER_SIZE 20000
char xprintf_buffer[XPRINTF_BUFFER_SIZE];

struct _tagged_string *xprintf(char *fmt, ...) {
  va_list argp;
  int len1;
  int len2;
  struct _tagged_string *result;

  va_start(argp,fmt);
  // JGM:  I'm changing this for the moment because I don't have vsnprintf
  len1 = vsnprintf(NULL,0,fmt,argp); // how much space do we need
  //len1 = vsprintf(xprintf_buffer,fmt,argp);
  if (len1 >= XPRINTF_BUFFER_SIZE) {
    fprintf(stderr,"internal error: xprintf buffer too small\n");
    exit(1);
  }
  va_end(argp);
  // Presumably the Cyclone typechecker rules this out, but check anyway
  if (len1 < 0) {
    fprintf(stderr,"internal error: encoding error in xprintf\n");
    exit(1);
  }
  // Careful: we rely on new_string to return enough space for a
  // trailing zero here
  result = new_string(len1);
  va_start(argp,fmt);
  // JGM:  I'm changing this to sprintf for the moment because 
  // I don't have vsnprintf
  len2 = vsnprintf(result->contents,len1+1,fmt,argp);
  //len2 = vsprintf(result->contents,fmt,argp);
  va_end(argp);
  if (len1 != len2) {
    fprintf(stderr, "internal error: encoding error in xprintf\n");
    exit(1);
  }
  return result;
}

// Exceptions

static struct _handler_cons *_handler_stack = NULL;

// create a new handler, put it on the stack, and return it so its
// jmp_buf can be filled in by the caller
struct _handler_cons *_push_handler() {
  struct _handler_cons *h =
    (struct _handler_cons *)GC_malloc(sizeof(struct _handler_cons));
  if (h == NULL) {
    fprintf(stderr,"internal error: out of memory in _push_handler\n");
    exit(1);
  }
  h->tail = _handler_stack;
  _handler_stack = h;
  return h;
}

// Pop n handlers off the stack, then one more, returning it
// Invariant: result is non-null
struct _handler_cons *_npop_handler(int n) {
  struct _handler_cons *result;
  if (n<0) {
    fprintf(stderr,
            "internal error: _npop_handler called with negative argument\n");
    exit(1);
  }
  for (;n>=0;n--) {
    if (_handler_stack == NULL) {
      fprintf(stderr,"internal error: empty handler stack\n");
      exit(1);
    }
    result = _handler_stack;
    _handler_stack = (struct _handler_cons*)_handler_stack->tail;
  }
  return result;
}

static void _init_handler_stack() {
  exn e;
  struct _handler_cons *h =
    (struct _handler_cons *)GC_malloc(sizeof(struct _handler_cons));
  if (h == NULL) {
    fprintf(stderr,"internal error: out of memory in _init_handler_stack\n");
    exit(1);
  }
  h->tail = NULL;
  _handler_stack = h;
  e = (exn)setjmp(h->handler);
  if (e) {
    fprintf(stderr,"Uncaught exception %s\n",e->tag);
    exit(1);
  }
}

void _pop_handler() {
  _npop_handler(0);
}
void throw(exn e) {
  struct _handler_cons *h;
  // fprintf(stderr,"throwing exception %s\n",e->tag); fflush(stderr);
  h = _npop_handler(0);
  longjmp(h->handler,(int)e);
}
void _throw(exn e) {
  throw(e);
}

/*

PROBLEM: the strategy below does not work.  _trycatch is supposed to
return NULL the first time it returns, and, if it returns a second
time, it should return a non-NULL exn that was thrown.  In fact, this
is what happens.  HOWEVER, because of the way setjmp is implemented,
the second non-NULL return value does not make it into e.  The only
way I've found to get around this is to essentially inline the
_trycatch, but, this means exposing setjmp everywhere, so it's not
great.

exn _trycatch() {
  struct _handler_cons *h = _push_handler();
  return (exn)setjmp(h->handler);
}

// Now try works as follows:
exn e = _trycatch();
if (!e) {
  // try body
  _pop_handler();
} else {
  // handlers -- switch on e
} */


////////////////////////////////////////////////////////////////
// The rest of the code is stuff declared in core.h or otherwise
// used in core.c
// It's taken from talc/runtime/stdlib.c
////////////////////////////////////////////////////////////////

// The C include file precore_c.h is produced (semi) automatically
// from the Cyclone include file precore.h.
#include "precore_c.h"

struct _tagged_string *Cstring_to_string(Cstring s) {
  struct _tagged_string *str;
  int sz=(s?strlen(s):0);
  str = new_string(sz);

  while(--sz>=0)
    // Copy the string in case the C code frees it or mangles it
    str->contents[sz]=s[sz];

  return str;
}

Cstring string_to_Cstring(string s) {
  int i;
  char *contents;
  char *str=(char *)GC_malloc(s->sz+1);
  if (str == NULL) {
    fprintf(stderr,"internal error: out of memory in string_to_Cstring\n");
    exit(1);
  }
  contents = s->contents;

  for(i=0; i<s->sz; i++) str[i]=contents[i];
  str[s->sz]='\0';
  return str;
}

extern int system(Cstring);

static void check_fd(FILE *fd) {
  if(!fd) {
    fprintf(stderr,"Attempt to access null file descriptor.\n");
    exit(255);
  }
}
int f_string_read(FILE *fd, string dest, int dest_offset, int max_count) {
  check_fd(fd);
  if(dest_offset + max_count > dest->sz) {
    fprintf(stderr,"Attempt to read off end of string.\n");
    exit(255);
  }
  return fread((dest->contents)+dest_offset, 1, max_count, fd);
}

// extern int fflush(FILE *); // supplied by stdio
// extern int fgetc(FILE *);  // supplied by stdio


/////////////////////////////////////
// THIS SECTION ONLY USED IN CORE.CYC

// extern FILE    *fopen            (Cstring,Cstring);  // supplied by stdio
int f_close(FILE *fd) {
  if(!fd) {
    fprintf(stderr,"Attempt to close null file descriptor.\n");
    exit(255);
  }
  return fclose(fd);
}
///////////////////////////////////////////////


#include <stdio.h>

FILE *cyc_stdin = NULL;
FILE *cyc_stdout = NULL;
FILE *cyc_stderr = NULL;

void init_stdlib_io() {
  cyc_stdin = stdin;
  cyc_stdout = stdout;
  cyc_stderr = stderr;
}


// To be used in core.cyc only
int cyc_argc;
static char **cyc_argv;

static int current_argc;
static char **current_argv;

void start_args() {
  current_argc = cyc_argc;
  current_argv = cyc_argv;
}

struct _tagged_string *next_arg() {
  struct _tagged_string *arg;
  if (current_argc <= 0) {
    fprintf(stderr,"next_arg called when there is no next arg\n");
    exit(1);
  }
  arg = Cstring_to_string(*current_argv);
  current_argv++;
  current_argc--;
  return arg;
}

// struct array {
//   unsigned int sz;
// }
// void *std_args() {
//   void *result = new_array(tal_argc,(void *)0);
//   void **data = result->elts;
//   int arg = tal_argc;
//   while(arg--) 
//     data[arg]=new_string(strlen(tal_argv[arg]));
//   return result;
// }
// 
// 
// array   new_array(int size, void *init) { 
//   void **a=GC_malloc(size*4);
//   array na = GC_malloc(sizeof(struct arr_internal));
//   na->size=size;
//   na->elts=a;
//   if (init) {
//     void **a_end = a+size;
//     for(;a<a_end;a++)
//       *a=init;
//   }
//   return na;
// }


extern void cyc_main(void);

int main(int argc, char **argv)
{
  _init_handler_stack();
  init_stdlib_io();
  cyc_argc = argc;
  cyc_argv = argv;
  cyc_main();
  return 0;
}
