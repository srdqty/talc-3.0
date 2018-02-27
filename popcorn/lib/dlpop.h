#ifndef _DLPOP_H
#define _DLPOP_H

extern handle_t;

/* for single files */
extern handle_t dlopen(string);
extern handle_t dlopen_buf (string talbuf, string tobuf, string name);

/* for mutually recursive files */
extern handle_t dlopens(string filenames[]) [];
extern handle_t dlopen_bufs(*(string,string,string) objfiles []) [];

extern a dlsym<a> (handle_t x, string name, <a>rep typ);

extern void dlclose(handle_t);

extern exception WrongType(string);
extern exception FailsTypeCheck;
extern exception SymbolNotFound(string);
/* raised in the default functions of loadable code */
extern exception UnresolvedSymbol(string);

/* updating */
extern b updated_sym<a,b>(a oldval, <a>rep oldtyp, <b>rep newtyp);

/* debugging */
extern bool dlpop_verbose;
extern void print_table();
extern void print_handle_table (handle_t t);

/* extern bool dlinit (void init (a lsyml<a> (string, <*(a)>rep), 
   void asym<a> (string, <*(a)>rep, a))); */

#endif
