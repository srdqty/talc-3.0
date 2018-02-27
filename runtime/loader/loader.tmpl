/* Implementation of TAL LOAD macro and popcorn wrapper */
/* #define TIMEIT */
#include <assert.h>
#ifdef TIMEIT
#include <sys/time.h>
#include <unistd.h>
#endif

#include "obj.h"
#include "util.h"

/* to install a .o file in our address space & lookup symbols */
extern struct obj_file *load(char *buf, int buflen);
extern Addr lookup_symbol(struct obj_file *f, const char *symname);

/* for typechecking */
struct string_list {
  char *str;
  struct string_list *next;
};

extern struct string_list *
verify_tal_file (char *typrep, int typereplen,
		 char *tofile, int tofilelen,
		 char *codefile, int codefilelen);
extern int
eqconrep (char *trep1, int rep1len,
	  char *trep2, int rep2len);
      
typedef struct str_internal {int size; char *chars;} *string;
typedef struct rep_internal {int size; char chars[1];} *rep;

/* This is the implementation of the TAL LOAD macro */
Addr *tal_load(string objfilebuf, string tofilebuf, rep typerep) {
  
  char *Cobjfilebuf;
  int Cobjfilelen;
  char *Ctofilebuf; 
  int Ctofilelen;
  char *Ctyperepbuf; 
  int Ctypereplen;
  struct string_list *symbols;
#ifdef TIMEIT
  struct timeval b,a;
  long delta_sec, delta_usec;
#endif

  Cobjfilebuf = objfilebuf->chars;
  Cobjfilelen = objfilebuf->size;
  Ctofilebuf = tofilebuf->chars;
  Ctofilelen = tofilebuf->size;
  Ctyperepbuf = typerep->chars;
  Ctypereplen =  typerep->size;

  /* verify the file first */
  if ((symbols = verify_tal_file (Ctyperepbuf, Ctypereplen,
				  Ctofilebuf, Ctofilelen,
				  Cobjfilebuf, Cobjfilelen))) {
    
  
    int i, num_syms;
    struct string_list *tmp = symbols;
    Addr *symbol_tuple;
    Addr *ret_tuple;
    struct obj_file *f;
    
#ifdef TIMEIT
  gettimeofday(&b,NULL);
#endif
    /* then load it in the address space */
    if ((f = load(Cobjfilebuf,Cobjfilelen))) {
#ifdef TIMEIT
      gettimeofday(&a,NULL);
      delta_sec = a.tv_sec - b.tv_sec;
      delta_usec = a.tv_usec - b.tv_usec;
      if (delta_usec < 0) {
        delta_sec--; delta_usec += 1000000;
      }
      fprintf (stderr,"TIME load = %ld.%06ld secs\n",
	       delta_sec,delta_usec);
#endif

      /* construct a Popcorn tuple of all of the exported symbols;
         it is important that the order of symbols in this tuple
         matches the order indicated by the type of the caller */
      num_syms = 1;
      while ((tmp = tmp->next)) num_syms++;
      symbol_tuple = (Addr *)xmalloc(sizeof(Addr) * num_syms);

      for (tmp = symbols, i = 0; tmp != NULL; i++, tmp = tmp->next) {
	symbol_tuple[i] = lookup_symbol(f,tmp->str);
	assert(symbol_tuple[i] != (Addr)0);
      }
      ret_tuple = (Addr *)xmalloc(sizeof(Addr));
      *ret_tuple = (Addr)symbol_tuple;
      return ret_tuple;
    }
    else {
      error("Failed to load: malformed object file\n");
      return NULL;
    }
  }
  else {
    error("Failed to load: verification failed\n");
    return NULL;
  }
}

/* This is the implementation of the TAL CAST macro */
void **tal_cast(void* obj, rep trep1, rep trep2) {  
  char *Ctrep1buf;
  int Ctrep1len;
  char *Ctrep2buf; 
  int Ctrep2len;
  
  Ctrep1buf = trep1->chars;
  Ctrep1len = trep1->size;
  Ctrep2buf = trep2->chars;
  Ctrep2len = trep2->size;
  
  /* compare the types */
  if (eqconrep (Ctrep1buf, Ctrep1len, Ctrep2buf, Ctrep2len)) {
    void ** opt = (void **)xmalloc(sizeof(void*));
    opt[0] = obj;
    return opt;
  }
  else {
    return NULL;
  }
}

extern void print_conrep(char *buf, int len);
void tal_print_typerep(rep trep) {
  char *Ctrepbuf;
  int Ctreplen;

  Ctrepbuf = trep->chars;
  Ctreplen = trep->size;

  print_conrep(Ctrepbuf, Ctreplen);
} 
