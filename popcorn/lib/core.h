
////////////////////////////////////////////////////////////////////////////
// Popcorn library, file core.h                                           //
// Copyright Greg Morrisett, Dan Grossman                                 //
// January 1999, all rights reserved                                      //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#ifndef CORE_H
#define CORE_H

// this first block is actually written in C and is always linked in
// but clients don't need to know that.
extern FILE?;
extern Cstring?;
extern int errno;
extern int tal_errno;

extern Cstring string_to_Cstring(string);
extern int system(Cstring);
extern <string>array std_args();
extern <string>array std_env();
extern a new_array<a>(int,a)[];
extern int new_array4(int)[];
extern unsigned int new_array4u(int)[];
extern FILE tal_stdout;
extern FILE tal_stdin;
extern FILE tal_stderr;
extern void fprint_int    (FILE,int);
extern void fprint_uint   (FILE,unsigned int);
extern void fprint_string (FILE,string);
extern void fprint_char   (FILE,char);
extern void fprint_newline(FILE);
extern int  f_string_read (FILE,string,int,int);
extern void fflush(FILE);
extern char fgetc(FILE);

// allows code to print exception dumps
extern void pop_exn_info(FILE,exn);

// GC interface
extern void GC_gcollect();
extern unsigned int GC_gc_no;
extern int GC_quiet;
extern bool GC_expand_hp (int szb);
extern int GC_get_heap_size ();
/* #define DEBUG_MALLOC */
#ifdef DEBUG_MALLOC
extern void GC_generate_random_backtrace();
#endif
#ifdef FLOATS

extern float new_array_float(int)[];
extern void fprint_float  (FILE,float);
extern double new_array_double(int)[];
extern void fprint_double (FILE,double);
extern double arc_cos(double);
extern double arc_sin(double);
extern double power(double,double);

#endif

prefix Core {
  open Core {
// the rest is defined in core.pop
extern void print_int(int);
extern void print_string(string);
extern void print_newline();
extern void print_char(char);
extern exception FileOpenError(string);
extern exception FileCloseError;
extern FILE file_open(string,string);
extern void file_close(FILE);
extern int  file_delete(string);
extern int  file_length(string);
extern int  file_gets(string,int,FILE);
extern void fprint_hex (FILE,unsigned int);
extern string get_env(string);

#define EOF chr(-1)

extern ?struct <a>Opt { a v; }
extern <b>Opt opt_map<a,b>(b f(a), <a>Opt);
extern struct <a>ref { a v; }

extern bool true_f<a>(a);
extern bool false_f<a>(a);

extern a fst<a,b>(*(a,b));
extern b snd<a,b>(*(a,b));
extern c third<a,b,c>(*(a,b,c));

extern a identity<a>(a);

extern int intcmp(int,int);
extern int charcmp(char,char);
extern int strcmp(string,string);
extern string strconcat(string,string);
extern string strcpy(string);
extern void strncpy(string,int,string,int,int);
extern string new_string(int);

extern exception InvalidArg(string);
extern exception Failure(string);
extern exception Not_found;
extern exception Impossible(string);

extern bool is_space(char);

extern int    int_of_string(string);
extern string string_of_int_width(int n, int minWidth);
extern string string_of_int(int);
extern string string_of_uint(unsigned int);
extern string hex_string_of_uint(unsigned int);
extern string string_of_char(char);

extern float float_of_string(string);
extern string string_of_float(float);
extern double double_of_string(string);
extern string string_of_double(double);

extern string hexstr_of_double(double);
extern string hexstr_of_float(float); 
}}

#endif
