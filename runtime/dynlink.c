#include <string.h>

#include "caml/mlvalues.h"
#include "caml/callback.h"
#include "caml/memory.h"
#include "caml/alloc.h"

extern void *GC_malloc(int size);

#ifdef __linux__
#define CONVERT_SYM(s) ((s)[0] == '_' ? &((s)[1]) : (s))
#else
#define CONVERT_SYM(s) (s)    
#endif

struct string_list {
  char *str;
  struct string_list *next;
};

/* C interface for caml function verify_tal_file, in dynlink.ml.
   We basically need to copy the passed C strings into suitable
   CAML representations (allocated in the Caml heap -- this is
   important), call the function, and then return its result. */

struct string_list *
verify_tal_file (char *typerep, int typereplen,
		 char *tofile, int tofilelen,
		 char *codefile, int codefilelen)
{
  CAMLparam0();
  CAMLlocal4(Caml_typerep,Caml_tofile,Caml_codefile,answer);

  static value *verify_tal_file_closure = NULL;
  struct string_list *retval = NULL;
 
  Caml_typerep = alloc_string(typereplen);
  Caml_tofile = alloc_string(tofilelen);
  Caml_codefile = alloc_string(codefilelen);

  memcpy(String_val(Caml_typerep),typerep,typereplen);
  memcpy(String_val(Caml_tofile),tofile,tofilelen);
  memcpy(String_val(Caml_codefile),codefile,codefilelen);

  if (verify_tal_file_closure == NULL)
    verify_tal_file_closure = caml_named_value ("verify_tal_file");
    
  answer = callback3(*verify_tal_file_closure, 
		     Caml_typerep, Caml_tofile, Caml_codefile);

  /* process the results -- should be a string list option */
  /* convert it to a C struct */
  
  if (Is_block(answer)) {
    value list = Field(answer, 0);
    struct string_list tmp;
    struct string_list *ptmp = &tmp;
    while (Is_block(list)) {
      ptmp->next = GC_malloc(sizeof(struct string_list));
      ptmp = ptmp->next;
      if (retval == NULL) {
	retval = ptmp;
      }
      ptmp->str = CONVERT_SYM((char *)(Field(list, 0)));
      list = Field(list,1);
    }
  }
  /* else None */

  CAMLreturn(retval);
}

int 
eqconrep (char *trep1, int trep1len,
	  char *trep2, int trep2len)
{
  CAMLparam0(); 
  CAMLlocal3(Caml_trep1, Caml_trep2, answer); 
  static value *cast_closure = NULL; 
  static int retval = 0; 

  Caml_trep1 = alloc_string(trep1len); 
  Caml_trep2 = alloc_string(trep2len);  
  memcpy (String_val(Caml_trep1), trep1, trep1len); 
  memcpy (String_val(Caml_trep2), trep2, trep2len); 
   
  if (cast_closure == NULL) 
    cast_closure = caml_named_value ("eqconrep");
  answer = callback2(*cast_closure, Caml_trep1, Caml_trep2); 
  if (answer == Val_int(1)) retval = 1; 
  else retval = 0; 
  CAMLreturn(retval);
} 

void print_conrep(char *rep, int replen)
{
  CAMLparam0(); 
  CAMLlocal2(Caml_trep, answer); 
  static value *cast_closure = NULL; 
  static int retval = 0; 

  Caml_trep = alloc_string(replen); 
  memcpy (String_val(Caml_trep), rep, replen); 
   
  if (cast_closure == NULL) 
    cast_closure = caml_named_value ("print_conrep");
  answer = callback(*cast_closure, Caml_trep); 
  CAMLreturn0;
}

struct string {
  char *str;
  int len;
};

value init_array_elem(struct string *elem)
{
  CAMLparam0(); 
  CAMLlocal1(Caml_context); 
  Caml_context = alloc_string(elem->len);
  memcpy (String_val(Caml_context), elem->str, elem->len); 
  CAMLreturn(Caml_context);
}

int 
register_context (struct string *contexts[])
{
  CAMLparam0(); 
  CAMLlocal2(Caml_contexts, answer); 
  static value *register_context_closure = NULL; 
  static int retval = 0; 

  if (register_context_closure == NULL) 
    register_context_closure = caml_named_value ("register_context");
  
  /* construct a Caml list for the contexts */
  Caml_contexts = alloc_array((value (*)(char *))init_array_elem,
                              (char **)contexts);
  answer = callback(*register_context_closure, Caml_contexts); 
  if (answer == Val_int(1)) retval = 1; 
  else retval = 0; 
  CAMLreturn(retval);
} 

#ifdef __linux__

#include <sys/types.h>
#include <sys/time.h>

value caml_gettimeofday(value unit)                /* ML */
{
  struct timeval tp;
  if (gettimeofday(&tp, NULL) == -1) return copy_double((double)-1);
  return copy_double((double) tp.tv_sec + (double) tp.tv_usec / 1e6);
}

#else /* no gettimeofday----may or may not exist for win32 */

value unix_gettimeofday(value unit)                /* ML */
{
  return copy_double((double)-1);
}

#endif


/************************************************************/
/************** worry about this later ... ******************/
/************************************************************/

/***************************
  The hard part with interfacing with Caml is figuring out
  what the format of the data they want is, without copying it
  a bunch of times. The caml documentation is a little spare.
  These probably don't work, but I'm working on them.
  
typedef struct {int size; char chars[1];} *popstring;

value pop2camlstring(popstring s){
  s->size = s->size << 10;
  s->size = (s->size | String_tag);
  return (value)s;
}

popstring caml2popstring(value s){
  if (Tag_val(s) == String_tag) {
    popstring s2;
    s2->size = string_length(s);
    s2->chars = String_val(s);
    return s2;
  } else {
    exit(0);
    return null;
  }
}

value pop2camlstring(popstring s){
  return copy_string (cstring2Cstring(s));
}

***************************/

